#include <string>
#include <cstdio>
#include <cctype>
#include <memory>
#include <vector>
#include <map>
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/StandardInstrumentations.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Scalar/Reassociate.h"
#include "llvm/Transforms/Scalar/SimplifyCFG.h"
// The lexer returns tokens [0-255] if it is an unknown character (ASCII character), otherwise one
// of these for known things.
enum Token {
    tok_eof = -1,
    // commands
    tok_def = -2,
    tok_extern = -3,
    // primary
    tok_identifier = -4,
    tok_number = -5,
    // control
    tok_if = -6,
    tok_then = -7,
    tok_else = -8,
};

static std::string IdentifierStr; // Filled in if tok_identifier
static double NumVal; // Filled in if tok_number

// getNextTok - Return the next token from the stdin
static int getNextTok() {
    static int lastChar = ' ';
    while (std::isspace(lastChar)) 
        lastChar = std::getchar();

    if (std::isalpha(lastChar)) {
        // identifier: [a-zA-Z][a-zA-Z0-9]*
        IdentifierStr = lastChar;
        while (std::isalnum(lastChar = std::getchar()))
            IdentifierStr += lastChar;
        
        if (IdentifierStr == "def")
            return tok_def;
        if (IdentifierStr == "extern")
            return tok_extern;
        if (IdentifierStr == "if")
          return tok_if;
        if (IdentifierStr == "then")
          return tok_then;
        if (IdentifierStr == "else")
          return tok_else;
        return tok_identifier;
    }

    if (std::isdigit(lastChar) || lastChar == '.') {
        // misunderstanding some digits e.g. 1.23.64
        // but this stays now.
        std::string NumStr;
        do {
            NumStr += lastChar;
            lastChar = std::getchar();
        } while (std::isdigit(lastChar) || lastChar == '.');
        NumVal = std::strtod(NumStr.c_str(), nullptr);
        return tok_number;
    }

    if (lastChar == '#') {
        // comment until the end of line.
        do {
            lastChar = std::getchar();
        } while (lastChar != EOF && lastChar != '\n' && lastChar != '\r');

        if (lastChar != EOF) 
            return getNextTok();
    }

    if (lastChar == EOF)
        return tok_eof;
    // Otherwise, just return the character as its ascii value.
    int thisChar = lastChar;
    lastChar = std::getchar();
    return thisChar;
}

static std::unique_ptr<llvm::LLVMContext> TheContext;
static std::unique_ptr<llvm::IRBuilder<>> Builder;
static std::unique_ptr<llvm::Module> TheModule;
static std::map<std::string, llvm::Value *> NamedValues;
llvm::Value *LogErrorV(const char *Str);
static std::unique_ptr<llvm::FunctionPassManager> TheFPM;
static std::unique_ptr<llvm::LoopAnalysisManager> TheLAM;
static std::unique_ptr<llvm::FunctionAnalysisManager> TheFAM;
static std::unique_ptr<llvm::CGSCCAnalysisManager> TheCGAM;
static std::unique_ptr<llvm::ModuleAnalysisManager> TheMAM;
static std::unique_ptr<llvm::PassInstrumentationCallbacks> ThePIC;
static std::unique_ptr<llvm::StandardInstrumentations> TheSI;

// AST
class ExprAST {
public:
    virtual ~ExprAST() = default;
    virtual llvm::Value *codegen() = 0;
};

class NumberExprAST : public ExprAST {
    double Val;
public:
    NumberExprAST(double Val) : Val(Val) {}
    llvm::Value *codegen() override;
};

/// VariableExprAST - Expression class for referencing a variable, like "a".
class VariableExprAST : public ExprAST {
  std::string Name;

public:
  VariableExprAST(const std::string &Name) : Name(Name) {}
  llvm::Value *codegen() override;
};

/// BinaryExprAST - Expression class for a binary operator.
class BinaryExprAST : public ExprAST {
    char Op;
    std::unique_ptr<ExprAST> LHS, RHS; // Only BinaryExprAST has the pointer of them.
public:
    // use move for rigid memory management
    BinaryExprAST(char Op, std::unique_ptr<ExprAST> LHS, std::unique_ptr<ExprAST> RHS) : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
    llvm::Value *codegen() override;
};

/// CallExprAST - Expression class for function calls.
class CallExprAST : public ExprAST {
  std::string Callee;
  std::vector<std::unique_ptr<ExprAST>> Args;

public:
  CallExprAST(const std::string &Callee,
              std::vector<std::unique_ptr<ExprAST>> Args)
    : Callee(Callee), Args(std::move(Args)) {}
  llvm::Value *codegen() override;
};

/// IfExprAST - Expression class for if/then/else.
class IfExprAST : public ExprAST {
  std::unique_ptr<ExprAST> Cond, Then, Else;

public:
  IfExprAST(std::unique_ptr<ExprAST> Cond, std::unique_ptr<ExprAST> Then, std::unique_ptr<ExprAST> Else) : Cond(std::move(Cond)), Then(std::move(Then)), Else(std::move(Else)) {}

  llvm::Value *codegen() override;
};
/// PrototypeAST - This class represents the "prototype" for a function,
/// which captures its name, and its argument names (thus implicitly the number
/// of arguments the function takes).
class PrototypeAST {
  std::string Name;
  std::vector<std::string> Args;

public:
  PrototypeAST(const std::string &Name, std::vector<std::string> Args)
    : Name(Name), Args(std::move(Args)) {}

  const std::string &getName() const { return Name; }
  llvm::Function *codegen();
};

/// FunctionAST - This class represents a function definition itself.
class FunctionAST {
  std::unique_ptr<PrototypeAST> Proto;
  std::unique_ptr<ExprAST> Body;

public:
  FunctionAST(std::unique_ptr<PrototypeAST> Proto,
              std::unique_ptr<ExprAST> Body)
    : Proto(std::move(Proto)), Body(std::move(Body)) {}
  llvm::Function *codegen();
};

static int curTok; // curTok - Provide a simple token buffer.  curTok is the current token the parser is looking at.
/// LogError* - These are little helper functions for error handling.
std::unique_ptr<ExprAST> LogError(const char *Str) {
  std::fprintf(stderr, "Error: %s\n", Str);
  return nullptr;
}
std::unique_ptr<PrototypeAST> LogErrorP(const char *Str) {
  LogError(Str);
  return nullptr;
}

llvm::Value *LogErrorV(const char *Str) {
  LogError(Str);
  return nullptr;
}

// Formula validation
static std::unique_ptr<ExprAST> ParseNumberExpr();
static std::unique_ptr<ExprAST> ParseParenExpr();
static std::unique_ptr<ExprAST> ParseIdentifierExpr();
static std::unique_ptr<ExprAST> ParsePrimary();
static int GetTokPrecedence();
static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec, std::unique_ptr<ExprAST> LHS);
static std::unique_ptr<ExprAST> ParseExpression();
// numberexpr := number
static std::unique_ptr<ExprAST> ParseNumberExpr() {
    auto result = std::make_unique<NumberExprAST>(NumVal); 
    curTok = getNextTok(); // consume the number
    return std::move(result);
}

// parenexpr := '('expression')'
static std::unique_ptr<ExprAST> ParseParenExpr() {
    curTok = getNextTok(); // consume (
    auto v = ParseExpression();
    if (!v)
        return nullptr;
    if (curTok != ')')
        return LogError("expected ')'");
    curTok = getNextTok(); // consume )
    return v;
}

/// identifierexpr
///   ::= identifier
///   ::= identifier '(' expression* ')'
static std::unique_ptr<ExprAST> ParseIdentifierExpr() {
  std::string IdName = IdentifierStr;

  curTok = getNextTok();  // eat identifier.

  if (curTok != '(') // Simple variable ref.
    return std::make_unique<VariableExprAST>(IdName);

  // Call.
  curTok = getNextTok();  // eat (
  std::vector<std::unique_ptr<ExprAST>> Args;
  if (curTok != ')') {
    while (true) {
      if (auto Arg = ParseExpression())
        Args.push_back(std::move(Arg));
      else
        return nullptr;

      if (curTok == ')')
        break;

      if (curTok != ',')
        return LogError("Expected ')' or ',' in argument list");
      curTok = getNextTok();
    }
  }

  // Eat the ')'.
  curTok = getNextTok();

  return std::make_unique<CallExprAST>(IdName, std::move(Args));
}


/// ifexpr ::= 'if' expression 'then' expression 'else' expression
static std::unique_ptr<ExprAST> ParseIfExpr() {
  curTok = getNextTok(); // consume if

  // condition
  auto Cond = ParseExpression(); // if expression then
  if (!Cond)
    return nullptr;

  if (curTok != tok_then)
    return LogError("expected then");
  curTok = getNextTok();

  auto Then = ParseExpression();
  if (!Then)
    return nullptr;

  if (curTok != tok_else)
    return LogError("expected else");

  curTok = getNextTok();

  auto Else = ParseExpression();
  if (!Else)
    return nullptr;

  return std::make_unique<IfExprAST>(std::move(Cond), std::move(Then), std::move(Else));
}

/// primary
///   ::= identifierexpr
///   ::= numberexpr
///   ::= parenexpr
static std::unique_ptr<ExprAST> ParsePrimary() {
  switch (curTok) {
  default:
    return LogError("unknown token when expecting an expression");
  case tok_identifier:
    return ParseIdentifierExpr();
  case tok_number:
    return ParseNumberExpr();
  case '(':
    return ParseParenExpr();
  case tok_if:
    return ParseIfExpr();
  }
}

// binary expressions
// Use Operator-precedence parser

/// BinopPrecedence - This holds the precedence for each binary operator that is
/// defined.
static std::map<char, int> BinopPrecedence;

/// GetTokPrecedence - Get the precedence of the pending binary operator token.
static int GetTokPrecedence() {
  if (!isascii(curTok))
    return -1;

  // Make sure it's a declared binop.
  int TokPrec = BinopPrecedence[curTok];
  if (TokPrec <= 0) return -1;
  return TokPrec;
}

/// binoprhs
///   ::= ('+' primary)*
// Very difficult to read
static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec, std::unique_ptr<ExprAST> LHS) {
  
  // If this is a binop, find its precedence.
  while (true) {
    int TokPrec = GetTokPrecedence();

    // If this is a binop that binds at least as tightly as the current binop,
    // consume it, otherwise we are done.
    if (TokPrec < ExprPrec)
      return LHS;
    // Parse current expressions because TokPrec >= ExprPrec.
    // Okay, we know this is a binop.
    int BinOp = curTok;
    curTok = getNextTok(); // eat binop

    // Parse the primary expression after the binary operator.
    auto RHS = ParsePrimary();
    if (!RHS)
      return nullptr;

    // If BinOp binds less tightly with RHS than the operator after RHS, let
    // the pending operator take RHS as its LHS.
    int NextPrec = GetTokPrecedence();
    if (TokPrec < NextPrec) {
      // LHS binop(TokPrec) RHS binop(NextPrec)
      // curTok = binop(NextPrec)
      RHS = ParseBinOpRHS(TokPrec + 1, std::move(RHS));
      if (!RHS)
        return nullptr;
    }

    // Merge LHS/RHS.
    // Create new LHS
    LHS = std::make_unique<BinaryExprAST>(BinOp, std::move(LHS), std::move(RHS));
  }
}

/// expression
///   ::= primary binoprhs
///
static std::unique_ptr<ExprAST> ParseExpression() {
  auto LHS = ParsePrimary();
  if (!LHS)
    return nullptr;

  return ParseBinOpRHS(0, std::move(LHS));
}

/// prototype
///   ::= id '(' id* ')'
static std::unique_ptr<PrototypeAST> ParsePrototype() {
  if (curTok != tok_identifier)
    return LogErrorP("Expected function name in prototype");

  std::string FnName = IdentifierStr;
  curTok = getNextTok();

  if (curTok != '(')
    return LogErrorP("Expected '(' in prototype");

  // Read the list of argument names.
  std::vector<std::string> ArgNames;
  curTok = getNextTok();
  while (curTok == tok_identifier) {
    ArgNames.push_back(IdentifierStr);
    curTok = getNextTok();
  }
  if (curTok != ')')
    return LogErrorP("Expected ')' in prototype");

  // success.
  curTok = getNextTok();  // eat ')'.

  return std::make_unique<PrototypeAST>(FnName, std::move(ArgNames));
}

/// definition ::= 'def' prototype expression
static std::unique_ptr<FunctionAST> ParseDefinition() {
  curTok = getNextTok();  // eat def.
  auto Proto = ParsePrototype();
  if (!Proto) return nullptr;

  if (auto E = ParseExpression())
    return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
  return nullptr;
}

/// external ::= 'extern' prototype
static std::unique_ptr<PrototypeAST> ParseExtern() {
  curTok = getNextTok();  // eat extern.
  return ParsePrototype();
}

/// toplevelexpr ::= expression
static std::unique_ptr<FunctionAST> ParseTopLevelExpr() {
  if (auto E = ParseExpression()) {
    // Make an anonymous proto.
    auto Proto = std::make_unique<PrototypeAST>("", std::vector<std::string>());
    return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
  }
  return nullptr;
}

// codegen
llvm::Value *NumberExprAST::codegen() {
  return llvm::ConstantFP::get(*TheContext, llvm::APFloat(Val));
}

llvm::Value *VariableExprAST::codegen() {
  llvm::Value *v = NamedValues[Name];
  if (!v)
    LogErrorV("Unknown variable name");
  return v;
}

llvm::Value *BinaryExprAST::codegen() {
  llvm::Value *L = LHS->codegen();
  llvm::Value *R = RHS->codegen();
  if (!L || !R)
    return nullptr;
  switch (Op) {
  case '+':
    return Builder->CreateFAdd(L, R, "addtmp");
  case '-':
    return Builder->CreateFSub(L, R, "subtmp");
  case '*':
    return Builder->CreateFMul(L, R, "multmp");
  case '<':
    L = Builder->CreateFCmpULT(L, R, "cmptmp");
    // Convert bool 0/1 to double 0.0 or 1.0
    return Builder->CreateUIToFP(L, llvm::Type::getDoubleTy(*TheContext), "booltmp");
  default:
    return LogErrorV("invalid binary operator");
  }
}

llvm::Value *CallExprAST::codegen() {
  // Look up the name in the global module table:
  llvm::Function *CalleeF = TheModule->getFunction(Callee);
  if (!CalleeF)
    return LogErrorV("Unknown function referenced");
  
  // if argument mismatch error
  if (CalleeF->arg_size() != Args.size())
    return LogErrorV("Incorrect # arguments passed");

  std::vector<llvm::Value *> ArgsV;
  for (unsigned i = 0, e = Args.size(); i != e; ++i) {
    ArgsV.push_back(Args[i]->codegen()); // Args[i]: ExprAST
    if (!ArgsV.back())
      return nullptr;
  }

  return Builder->CreateCall(CalleeF, ArgsV, "calltmp");
}

llvm::Function *PrototypeAST::codegen() {
  // Make the dunction type: double(double, double) etc
  std::vector<llvm::Type*> Doubles(Args.size(), llvm::Type::getDoubleTy(*TheContext)); // double arguments
  llvm::FunctionType *FT = llvm::FunctionType::get(llvm::Type::getDoubleTy(*TheContext), Doubles, false);
  llvm::Function *F = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, Name, TheModule.get());

  int idx = 0;
  for (auto &Arg : F->args()) {
    Arg.setName(Args[idx++]);
  }
  return F;
}

llvm::Function *FunctionAST::codegen() {
  // First, check for an existing function from a previous 'extern' declaration.
  llvm::Function *TheFunction = TheModule->getFunction(Proto->getName());
  if (!TheFunction)
    TheFunction = Proto->codegen();
  
  if (!TheFunction)
    return nullptr;

  if (!TheFunction->empty())
    return (llvm::Function*)LogErrorV("Function cannot be redefined.");

  // Create a new basic block to start insertion into.
  // BasicBlock contains the elements of processing with no branch
  llvm::BasicBlock *BB = llvm::BasicBlock::Create(*TheContext, "entry", TheFunction);
  Builder->SetInsertPoint(BB);

  // Record the function arguments in the NamedValues map.
  NamedValues.clear();
  for (auto &Arg : TheFunction->args())
    NamedValues[std::string(Arg.getName())] = &Arg;
  
  if (llvm::Value *RetVal = Body->codegen()) {
    // Finish off the function.
    Builder->CreateRet(RetVal);

    // Validate the generated code, checking for consistency.
    llvm::verifyFunction(*TheFunction);

    // Optimize the function.
    TheFPM->run(*TheFunction, *TheFAM);

    return TheFunction;
  }

  // Error reading body, remove function.
  TheFunction->eraseFromParent();
  return nullptr;
}

// ifexpr codegen
llvm::Value *IfExprAST::codegen() {
  llvm::Value *CondV = Cond->codegen();
  if (!CondV)
    return nullptr;
  // Convert condition to a bool by comparing non-equal to 0.0
  CondV = Builder->CreateFCmpONE(CondV, llvm::ConstantFP::get(*TheContext, llvm::APFloat(0.0)), "ifcond");
  llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();

  // Create blocks for the then and else cases.  Insert the 'then' block at the
  // end of the function.
  llvm::BasicBlock *ThenBB = llvm::BasicBlock::Create(*TheContext, "then", TheFunction);
  llvm::BasicBlock *ElseBB = llvm::BasicBlock::Create(*TheContext, "else");
  llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(*TheContext, "ifcont");

  Builder->CreateCondBr(CondV, ThenBB, ElseBB);

  // Emit then value.
  Builder->SetInsertPoint(ThenBB);

  llvm::Value *ThenV = Then->codegen();
  if (!ThenV)
    return nullptr;

  Builder->CreateBr(MergeBB);
  // Codegen of 'Then' can change the current block, update ThenBB for the PHI.
  ThenBB = Builder->GetInsertBlock();

  // Emit else block.
  TheFunction->insert(TheFunction->end(), ElseBB);
  Builder->SetInsertPoint(ElseBB);

  llvm::Value *ElseV = Else->codegen();
  if (!ElseV)
    return nullptr;

  Builder->CreateBr(MergeBB);
  // codegen of 'Else' can change the current block, update ElseBB for the PHI.
  ElseBB = Builder->GetInsertBlock();

  // Emit merge block.
  TheFunction->insert(TheFunction->end(), MergeBB);
  Builder->SetInsertPoint(MergeBB);
  llvm::PHINode *PN = Builder->CreatePHI(llvm::Type::getDoubleTy(*TheContext), 2, "iftmp");

  PN->addIncoming(ThenV, ThenBB);
  PN->addIncoming(ElseV, ElseBB);
  return PN;
}

// top level parse

static void InitializeModule() {
  // Open a new context and module.
  TheContext = std::make_unique<llvm::LLVMContext>();
  TheModule = std::make_unique<llvm::Module>("my cool jit", *TheContext);

  // Create a new builder for the module.
  Builder = std::make_unique<llvm::IRBuilder<>>(*TheContext);

  // Create new pass and analysis managers.
  TheFPM = std::make_unique<llvm::FunctionPassManager>(); 
  TheLAM = std::make_unique<llvm::LoopAnalysisManager>(); // loop analysis
  TheFAM = std::make_unique<llvm::FunctionAnalysisManager>();
  TheCGAM = std::make_unique<llvm::CGSCCAnalysisManager>();
  TheMAM = std::make_unique<llvm::ModuleAnalysisManager>();
  ThePIC = std::make_unique<llvm::PassInstrumentationCallbacks>();
  TheSI = std::make_unique<llvm::StandardInstrumentations>(*TheContext,/*DebugLogging*/ true);
  TheSI->registerCallbacks(*ThePIC, TheMAM.get());

  // Add transform passes.
  // Do simple "peephole" optimizations and bit-twiddling optzns.
  TheFPM->addPass(llvm::InstCombinePass());
  // Reassociate expressions.
  TheFPM->addPass(llvm::ReassociatePass());
  // Eliminate Common SubExpressions.
  TheFPM->addPass(llvm::GVNPass());
  // Simplify the control flow graph (deleting unreachable blocks, etc).
  TheFPM->addPass(llvm::SimplifyCFGPass());

  // Register analysis passes used in these transform passes.
  llvm::PassBuilder PB;
  PB.registerModuleAnalyses(*TheMAM);
  PB.registerFunctionAnalyses(*TheFAM);
  PB.crossRegisterProxies(*TheLAM, *TheFAM, *TheCGAM, *TheMAM);

}

static void HandleDefinition() {
  if (auto FnAST = ParseDefinition()) {
    if (auto *FnIR = FnAST->codegen()) {
      fprintf(stderr, "Read function definition:");
      FnIR->print(llvm::errs());
      fprintf(stderr, "\n");
    }
  } else {
    // Skip token for error recovery.
    curTok = getNextTok();
  }
}

static void HandleExtern() {
  if (auto ProtoAST = ParseExtern()) {
    if (auto *FnIR = ProtoAST->codegen()) {
      fprintf(stderr, "Read extern: ");
      FnIR->print(llvm::errs());
      fprintf(stderr, "\n");
    }
  } else {
    // Skip token for error recovery.
    curTok = getNextTok();
  }
}

static void HandleTopLevelExpression() {
  // Evaluate a top-level expression into an anonymous function.
  if (auto FnAST = ParseTopLevelExpr()) {
    if (auto *FnIR = FnAST->codegen()) {
      fprintf(stderr, "Read top-level expression:");
      FnIR->print(llvm::errs());
      fprintf(stderr, "\n");

      // Remove the anonymous expression.
      FnIR->eraseFromParent();
    }
  } else {
    // Skip token for error recovery.
    curTok = getNextTok();
  }
}

/// top ::= definition | external | expression | ';'
static void MainLoop() {
  while (true) {
    fprintf(stderr, "ready> ");
    switch (curTok) {
    case tok_eof:
      return;
    case ';': // ignore top-level semicolons.
      curTok = getNextTok();
      break;
    case tok_def:
      HandleDefinition();
      break;
    case tok_extern:
      HandleExtern();
      break;
    default:
      HandleTopLevelExpression();
      break;
    }
  }
}



int main() {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();
    // Install standard binary operators.
    // 1 is lowest precedence.
    BinopPrecedence['<'] = 10;
    BinopPrecedence['+'] = 20;
    BinopPrecedence['-'] = 20;
    BinopPrecedence['*'] = 40;  // highest.
    // Prime the first token.
    fprintf(stderr, "ready> ");
    curTok = getNextTok();

    // Make the module, which holds all the code.
    InitializeModule();
    // Run the main "interpreter loop" now.
    MainLoop();

    // Print out all of the generated code.
    TheModule->print(llvm::errs(), nullptr);
    return 0;
}
