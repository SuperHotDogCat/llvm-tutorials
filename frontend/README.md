# t.ll fileから可視化する方法
```
opt -passes=dot-cfg t.ll     
dot -Tpdf .baz.dot -o cfg.pdf
```
