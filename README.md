# Oberon-forward-references-of-procedures
Modified Oberon-07 compiler that implements forward references of procedures for 2 use cases:

* **Use case A (default, includes use case B):** To implement forward *declarations* of procedures
* **Use case B (alternative, reduced):** To make *references* among nested procedures more efficient

Note: In this repository, the term "Project Oberon 2013" refers to a re-implementation of the original "Project Oberon" on an FPGA development board around 2013, as published at www.projectoberon.com.

**PREREQUISITES**: A current version of Project Oberon 2013 (see http://www.projectoberon.com) or Extended Oberon (see http://github.com/andreaspirklbauer/Oberon-extended).

------------------------------------------------------
**Use case A (default, includes use case B):** To implement both forward *references* of procedures **and** forward *declarations* of procedures, use source files *ORG.Mod* and *ORP.Mod*.

In this repository, this is the default use case. Note that with the changes needed for use case B, the implementation of forward *declarations* of procedures (use case A) comes practically for free. If you do not want to re-introduce forward *declarations* of procedures, but only forward *references* of procedures, use source files *ORG.Mod* and *ORP1.Mod* (use case B) and skip this section.

Forward declarations of procedures have been eliminated in the Oberon-07 language revision of 2013/2016 (for the full language report see http://inf.ethz.ch/personal/wirth/Oberon/Oberon07.Report.pdf).

The rationale was that forward declarations of procedures can always be eliminated from any program by an appropriate nesting or by introducing procedure variables (see ch. 2 of www.inf.ethz.ch/personal/wirth/Oberon/PortingOberon.pdf). Another argument for eliminating forward declaration of procedures is that the language already has the language construct of *procedure variables*, which are needed for handlers and call backs. Therefore, one doesn‘t *also* need the additional language construct of *forward declarations* of procedures.

Whether forward declarations of procedures *should* be re-introduced into the Oberon language, can be debated. Here, we have re-introduced them for three main reasons:

1. Direct procedure calls are more efficient than using procedure variables.
2. Legacy programs that contain forward references of procedures are now accepted again by the (modified) compiler.
3. Introducing forward declarations of procedures added only about 10 lines of source code (relative to use case B).

Forward declarations of procedures are implemented in exactly the same way as in the original implementation before the Oberon-07 language revision, i.e.

1. They are explicitly specified by ^ following the symbol PROCEDURE in the source text.
2. The compiler processes the heading in the normal way, assuming its body to be missing. The newly generated object in the symbol table is marked as a forward declaration.
3. When later in the source text the full declaration is encountered, the symbol table is first searched. If the given identifier is found and denotes a procedure, the full declaration is associated with the already existing entry in the symbol table and the parameter lists are compared. Otherwise a multiple definition of the same identifier is present.

------------------------------------------------------
**Use case B (alternative, reduced):** To make *references* among nested procedures more efficient, use source files *ORG.Mod* and *ORP1.Mod*.

In this repository, this is the alternative use case, which implements only forward *references*, but not forward *declarations* of procedures.

If a procedure Q which is local to another procedure P refers to the enclosing procedure P, as in

     PROCEDURE P;
       PROCEDURE Q;
       BEGIN (*body of Q*) P  (*forward reference from Q to P, as the body of P is not compiled yet*)
       END Q;
     BEGIN (*body of P*)
     END P;

then the official Oberon-07 compiler, as published on www.projectoberon.com, generates the following code:

     20  P'   BL  10         ; forward branch to line 31 (the body of P)
     21  Q    body of Q
              ...
              ...            ; any calls from Q to P are BACKWARD jumps to line 20 and from there forward to line 31
              ...
     31  P    body of P

whereas the modified compiler provided in **this** repository generates the following, more efficient, code:

     20  Q   body of Q
             ...
             ...             ; any calls from Q to P are FORWARD jumps to line 30, fixed up when the body of P is compiled
             ...
     30  P   body of P


i.e. it does **not** generate an extra forward jump in line 20 around Q to the body of P and backward jumps from Q to line 20. In Project Oberon 2013, the extra BL instruction in line 20 exists, so that Q can call P (Q is compiled before P).

------------------------------------------------------
**Implementation**

The modified compiler generates and fixes up forward references to a procedure P as follows:

1. When the *heading* of P is processed, the field *obj.type.len* is set to 0 to indicate that no forward reference to P has been generated yet, and *obj.val* is set to -1 to indicate that the body of P has not been compiled yet. See *ORP.ProcedureDecl*:

        proc.type.len := 0;   (*used as the heading of the fixup list for forward references to P*)
        proc.val := -1;       (*<0: body of P has not been compiled yet; otherwise: entry address of P*)

     The use of *obj.type.len* as the heading of the fixup list is acceptable, because every procedure object *obj* (of type *ORB.Object*) has its **own** type object *obj.type* (of type *ORB.Type*) and its field *obj.type.len* is not used otherwise.

     The field *obj.type.len* is available during code generation as the field *x.type.len* in source level items generated from the procedure object *obj* using procedure *ORG.MakeItem*, while the field *obj.val* is available as the field *x.a*.

2. If a procedure P, whose body has not been compiled yet, is assigned to a procedure variable, passed as parameter to a procedure or returned as the result of a function procedure, a forward reference in the form of a *register* instruction is generated that will eventually contain an instruction operand. This adds a single line to *ORG.load*:

       IF x.a < 0 THEN (*forward*) Put3(BL, 7, 0); Put1(Add, RH, LNK, x.type.len); x.type.len := pc-1

Here, the purpose of the first instruction generated (branch zero step forward) is to simply deposit the link address PC+1 in register LNK ("LNK := PC+1").

3. If a procedure P, whose body has not been compiled yet, is *called*, a forward reference in the form of a *branch* instruction is generated that will eventually contain the branch displacement. This adds a single line to *ORG.Call*:.

       IF x.a < 0 THEN (*forward*) Put3(BL, 7, x.type.len); x.type.len := pc-1

4. When the procedure *body* of P is *compiled*, all forward references to P are *fixed up* with the now known actual entry address of P, and the field *obj.val* is changed to that address.

   This changes *ORP.ProcedureDecl* from:

        proc.val := ORG.Here() * 4; proc.type.dsc := ORB.topScope.next;
        IF sym = ORS.procedure THEN
          L := 0; ORG.FJump(L);
          REPEAT ProcedureDecl; Check(ORS.semicolon, "no ;") UNTIL sym # ORS.procedure;
          ORG.FixOne(L); proc.val := ORG.Here() * 4; proc.type.dsc := ORB.topScope.next
        END ;

   to:

        proc.type.dsc := ORB.topScope.next;
        WHILE sym = ORS.procedure DO ProcedureDecl; Check(ORS.semicolon, "no ;") END ;
        ORG.FixLink(proc.type.len);  (*fix forward references generated in ORG.load and ORG.Call*)
        proc.val := ORG.Here() * 4; proc.type.dsc := ORB.topScope.next; DEC(fc);

   As one can see, the forward jump at the beginning of P is no longer generated (*ORG.FJump* is not called).

   Note also that the second assignment *proc.type.dsc := ORB.topScope.next* is necessary to cover the case, where *proc.type.dsc* has been NIL before local procedures have been processed (an object for the local procedure will be added in that case).

5. Any references to P later in the source text are *backward* references using the actual entry address of P.

6. A single *fixup list* holding *both* register *and* branch instructions is constructed in *ORG.load* and *ORG.Call*, with its *links* placed in the instructions themselves in the place of the eventual instruction operands or branch displacements. This requires procedure *ORG.FixLink* to allow instructions of *different* instruction formats to be part of the *same* fixup list:

        PROCEDURE fix1(at, with: LONGINT);
          VAR v: LONGINT;
        BEGIN (*fix format-1 register instruction*)
          IF with < 0 THEN v := C28 (*set v bit*) ELSE v := 0 END ;
          code[at] := code[at] DIV C16 * C16 + (with MOD C16) + v
        END fix1;

        PROCEDURE fixlink(L, x: LONGINT);
          VAR L1, format: LONGINT;
        BEGIN (*fix format-1 and format-3 instructions in a single fixup chain*)
          WHILE L # 0 DO
            format := code[L] DIV C30 MOD 4; L1 := code[L] MOD C16;
            IF format = 1 THEN fix1(L, (x-L)*4)
            ELSIF format = 3 THEN fix(L, x-L-1)
            ELSE ORS.Mark("fixup impossible"); L1 := 0
            END ;
            L := L1
          END
        END fixlink;

        PROCEDURE FixLinkWith(L, dst: LONGINT); (*fixup with offset to dst*)
        BEGIN fixlink(L, dst)
        END FixLinkWith;

It is *essential* that the origin of the fixup list for P is **not** rooted in a variable *L* local to the parsing procedure *ProcedureDecl* (as is typical in other parsing procedures), because forward references may be generated from *other* procedures in the surrounding scope, the same scope or from within a nested scope (as in the example above). However, the fixup list must be associated with P at all times. A field in the symbol table entry for P is ideally suited for this purpose.

Note that our implementation both global *and* local procedures can be declared forward.

------------------------------------------------------
**Preparing your compiler to support forward references of procedures**

Convert the downloaded files to Oberon format (Oberon uses CR as line endings) using the command [**dos2oberon**](dos2oberon), also available in this repository (example shown for Mac or Linux):

     for x in *.Mod ; do ./dos2oberon $x $x ; done

Import the files to your Oberon system. If you use an emulator (e.g., **https://github.com/pdewacht/oberon-risc-emu**) to run the Oberon system, click on the *PCLink1.Run* link in the *System.Tool* viewer, copy the files to the emulator directory, and execute the following command on the command shell of your host system:

     cd oberon-risc-emu
     for x in *.Mod ; do ./pcreceive.sh $x ; sleep 0.5 ; done

Rebuild the Oberon compiler, depending on the use case that you want to implement:

     ORP.Compile ORB.Mod/s ORG.Mod/s ORP.Mod ~     # build a compiler for use case A
     ORP.Compile ORG.Mod/s ORP1.Mod ~              # build a compiler for use case B

     System.Free ORTool ORP ORG ORB ORS ~          # unload the old compiler used to build the new one

See the test programs *Test.Mod* and *Test1.Mod* for the correct compilation order.

# Appendix 1: Changes made to Project Oberon 2013 for use case A

**ORB.Mod**

```diff
--- FPGAOberon2013/ORB.Mod	2019-03-05 09:52:30.000000000 +0100
+++ Oberon-forward-references-of-procedures/Sources/FPGAOberon2013/ORB.Mod	2019-11-09 16:56:50.000000000 +0100
@@ -1,4 +1,4 @@
-MODULE ORB;   (*NW 25.6.2014  / 1.3.2019  in Oberon-07*)
+MODULE ORB;   (*NW 25.6.2014  / 1.3.2019  in Oberon-07 / AP 1.11.19*)
   IMPORT Files, ORS;
   (*Definition of data types Object and Type, which together form the data structure
     called "symbol table". Contains procedures for creation of Objects, and for search:
@@ -79,6 +79,13 @@
     END 
   END NewObj;
 
+  PROCEDURE FindObj*(id: ORS.Ident; list: Object): Object;  (*search id in list*)
+    VAR x: Object;
+  BEGIN x := list;
+    WHILE (x # NIL) & (x.name # id) DO x := x.next END ;
+    RETURN x
+  END FindObj;
+
   PROCEDURE thisObj*(): Object;
     VAR s, x: Object;
   BEGIN s := topScope;
```

**ORG.Mod**

```diff
--- FPGAOberon2013/ORG.Mod	2019-05-30 17:58:14.000000000 +0200
+++ Oberon-forward-references-of-procedures/Sources/FPGAOberon2013/ORG.Mod	2019-11-09 14:30:05.000000000 +0100
@@ -1,4 +1,4 @@
-MODULE ORG; (* N.Wirth, 16.4.2016 / 4.4.2017 / 31.5.2019  Oberon compiler; code generator for RISC*)
+MODULE ORG; (* N.Wirth, 16.4.2016 / 4.4.2017 / 17.9.2018  Oberon compiler; code generator for RISC / AP 15.5.19*)
   IMPORT SYSTEM, Files, ORS, ORB;
   (*Code generator for Oberon compiler for RISC processor.
      Procedural interface to Parser OSAP; result in array "code".
@@ -7,7 +7,7 @@
   CONST WordSize* = 4;
     StkOrg0 = -64; VarOrg0 = 0;  (*for RISC-0 only*)
     MT = 12; SP = 14; LNK = 15;   (*dedicated registers*)
-    maxCode = 8000; maxStrx = 2400; maxTD = 160; C24 = 1000000H;
+    maxCode = 8800; maxStrx = 3200; maxTD = 160; C16 = 10000H; C24 = 1000000H; C28 = 10000000H; C30 = 40000000H;
     Reg = 10; RegI = 11; Cond = 12;  (*internal item modes*)
 
   (*frequently used opcodes*)  U = 2000H; V = 1000H;
@@ -117,21 +117,34 @@
   BEGIN fix(at, pc-at-1)
   END FixOne;
 
-  PROCEDURE FixLink*(L: LONGINT);
-    VAR L1: LONGINT;
-  BEGIN
-    WHILE L # 0 DO L1 := code[L] MOD 40000H; fix(L, pc-L-1); L := L1 END
-  END FixLink;
-
-  PROCEDURE FixLinkWith(L0, dst: LONGINT);
-    VAR L1: LONGINT;
-  BEGIN
-    WHILE L0 # 0 DO
-      L1 := code[L0] MOD C24;
-      code[L0] := code[L0] DIV C24 * C24 + ((dst - L0 - 1) MOD C24); L0 := L1
+  PROCEDURE fix1(at, with: LONGINT);
+    VAR v: LONGINT;
+  BEGIN (*fix format-1 register instruction*)
+    IF with < 0 THEN v := C28 (*set v bit*) ELSE v := 0 END ;
+    code[at] := code[at] DIV C16 * C16 + (with MOD C16) + v
+  END fix1;
+
+  PROCEDURE fixlink(L, x: LONGINT);
+    VAR L1, format: LONGINT;
+  BEGIN (*fix format-1 and format-3 instructions in a single fixup chain*)
+    WHILE L # 0 DO
+      format := code[L] DIV C30 MOD 4; L1 := code[L] MOD C16;
+      IF format = 1 THEN fix1(L, (x-L)*4)
+      ELSIF format = 3 THEN fix(L, x-L-1)
+      ELSE ORS.Mark("fixup impossible"); L1 := 0
+      END ;
+      L := L1
     END
+  END fixlink;
+
+  PROCEDURE FixLinkWith(L, dst: LONGINT); (*fixup with offset to dst*)
+  BEGIN fixlink(L, dst)
   END FixLinkWith;
 
+  PROCEDURE FixLink*(L: LONGINT); (*fixup with offset to pc*)
+  BEGIN fixlink(L, pc)
+  END FixLink;
+
   PROCEDURE merged(L0, L1: LONGINT): LONGINT;
     VAR L2, L3: LONGINT;
   BEGIN 
@@ -162,7 +175,8 @@
     IF x.mode # Reg THEN
       IF x.mode = ORB.Const THEN
         IF x.type.form = ORB.Proc THEN
-          IF x.r > 0 THEN ORS.Mark("not allowed")
+          IF x.a < 0 THEN (*forward*) Put3(BL, 7, 0); Put1(Add, RH, LNK, x.type.len); x.type.len := pc-1
+          ELSIF x.r > 0 THEN ORS.Mark("not allowed")
           ELSIF x.r = 0 THEN Put3(BL, 7, 0); Put1a(Sub, RH, LNK, pc*4 - x.a)
           ELSE GetSB(x.r); Put1(Add, RH, RH, x.a + 100H) (*mark as progbase-relative*)
           END
@@ -771,7 +785,8 @@
   PROCEDURE Call*(VAR x: Item; r: LONGINT);
   BEGIN (*x.type.form = ORB.Proc*)
     IF x.mode = ORB.Const THEN
-      IF x.r >= 0 THEN Put3(BL, 7, (x.a DIV 4)-pc-1)
+      IF x.a < 0 THEN (*forward*) Put3(BL, 7, x.type.len); x.type.len := pc-1
+      ELSIF x.r >= 0 THEN Put3(BL, 7, (x.a DIV 4)-pc-1)
       ELSE (*imported*)
         IF pc - fixorgP < 1000H THEN
           Put3(BL, 7, ((-x.r) * 100H + x.a) * 1000H + pc-fixorgP); fixorgP := pc-1
```

**ORP.Mod**

```diff
--- FPGAOberon2013/ORP.Mod	2019-05-30 17:57:33.000000000 +0200
+++ Oberon-forward-references-of-procedures/Sources/FPGAOberon2013/ORP.Mod	2019-11-09 16:57:00.000000000 +0100
@@ -1,4 +1,4 @@
-MODULE ORP; (*N. Wirth 1.7.97 / 31.5.2019  Oberon compiler for RISC in Oberon-07*)
+MODULE ORP; (*N. Wirth 1.7.97 / 17.9.2018 / AP 15.5.19  Oberon compiler for RISC in Oberon-07*)
   IMPORT Texts, Oberon, ORS, ORB, ORG;
   (*Author: Niklaus Wirth, 2014.
     Parser of Oberon-RISC compiler. Uses Scanner ORS to obtain symbols (tokens),
@@ -12,7 +12,7 @@
     END ;
   
   VAR sym: INTEGER;   (*last symbol read*)
-    dc: LONGINT;    (*data counter*)
+    dc, fc: LONGINT;    (*data counter, forward counter*)
     level, exno, version: INTEGER;
     newSF: BOOLEAN;  (*option flag*)
     expression: PROCEDURE (VAR x: ORG.Item);  (*to avoid forward reference*)
@@ -849,46 +849,59 @@
     VAR proc: ORB.Object;
       type: ORB.Type;
       procid: ORS.Ident;
-      x: ORG.Item;
-      locblksize, parblksize, L: LONGINT;
-      int: BOOLEAN;
-  BEGIN (* ProcedureDecl *) int := FALSE; ORS.Get(sym); 
-    IF sym = ORS.times THEN ORS.Get(sym); int := TRUE END ;
-    IF sym = ORS.ident THEN
-      ORS.CopyId(procid); ORS.Get(sym);
-      ORB.NewObj(proc, ORS.id, ORB.Const);
-      IF int THEN parblksize := 12 ELSE parblksize := 4 END ;
-      NEW(type); type.form := ORB.Proc; type.size := ORG.WordSize;
-      proc.type := type; proc.val := -1; proc.lev := level; 
-      CheckExport(proc.expo);
-      IF proc.expo THEN proc.exno := exno; INC(exno) END ;
-      ORB.OpenScope; INC(level); type.base := ORB.noType;
-      ProcedureType(type, parblksize);  (*formal parameter list*)
-      Check(ORS.semicolon, "no ;"); locblksize := parblksize; 
-      Declarations(locblksize);
-      proc.val := ORG.Here() * 4; proc.type.dsc := ORB.topScope.next;
-      IF sym = ORS.procedure THEN
-        L := 0; ORG.FJump(L);
-        REPEAT ProcedureDecl; Check(ORS.semicolon, "no ;") UNTIL sym # ORS.procedure;
-        ORG.FixOne(L); proc.val := ORG.Here() * 4; proc.type.dsc := ORB.topScope.next
-      END ;
+      parblksize: LONGINT;
+      int, body, expo: BOOLEAN;
+
+    PROCEDURE Body(proc: ORB.Object; parblksize: LONGINT; int: BOOLEAN);
+      VAR x: ORG.Item; locblksize: LONGINT;
+    BEGIN
+      Check(ORS.semicolon, "no ;"); locblksize := parblksize;
+      Declarations(locblksize); proc.type.dsc := ORB.topScope.next;
+      WHILE sym = ORS.procedure DO ProcedureDecl; Check(ORS.semicolon, "no ;") END ;
+      ORG.FixLink(proc.type.len);  (*fix forward references generated in ORG.load and ORG.Call*)
+      proc.val := ORG.Here() * 4; proc.type.dsc := ORB.topScope.next; DEC(fc);
       ORG.Enter(parblksize, locblksize, int);
       IF sym = ORS.begin THEN ORS.Get(sym); StatSequence END ;
       IF sym = ORS.return THEN
         ORS.Get(sym); expression(x);
-        IF type.base = ORB.noType THEN ORS.Mark("this is not a function")
-        ELSIF ~CompTypes(type.base, x.type, FALSE) THEN ORS.Mark("wrong result type")
+        IF proc.type.base = ORB.noType THEN ORS.Mark("this is not a function")
+        ELSIF ~CompTypes(proc.type.base, x.type, FALSE) THEN ORS.Mark("wrong result type")
         END
-      ELSIF type.base.form # ORB.NoTyp THEN
-        ORS.Mark("function without result"); type.base := ORB.noType
+      ELSIF proc.type.base.form # ORB.NoTyp THEN
+        ORS.Mark("function without result"); proc.type.base := ORB.noType
       END ;
-      ORG.Return(type.base.form, x, locblksize, int);
-      ORB.CloseScope; DEC(level); Check(ORS.end, "no END");
+      ORG.Return(proc.type.base.form, x, locblksize, int); Check(ORS.end, "no END");
       IF sym = ORS.ident THEN
-        IF ORS.id # procid THEN ORS.Mark("no match") END ;
+        IF ORS.id # proc.name THEN ORS.Mark("no match") END ;
         ORS.Get(sym)
       ELSE ORS.Mark("no proc id")
       END
+    END Body;
+
+  BEGIN (* ProcedureDecl *) int := FALSE; body := TRUE; ORS.Get(sym);
+    IF sym = ORS.times THEN (*interrupt*) ORS.Get(sym); int := TRUE
+    ELSIF sym = ORS.arrow THEN (*forward*) ORS.Get(sym); body := FALSE
+    END ;
+    IF sym = ORS.ident THEN
+      ORS.CopyId(procid); ORS.Get(sym); CheckExport(expo);
+      IF int THEN parblksize := 12 ELSE parblksize := 4 END ;
+      NEW(type); type.form := ORB.Proc; type.size := ORG.WordSize; type.len := 0; (*heading of fixup list for forward refs*)
+      proc := ORB.FindObj(procid, ORB.topScope.next);
+      IF proc = NIL THEN  (*identifier not found in the symbol table*)
+        ORB.NewObj(proc, procid, ORB.Const); INC(fc);
+        proc.type := type; proc.val := -1; proc.lev := level; proc.expo := expo;
+        IF expo THEN proc.exno := exno; INC(exno) END
+      END ;
+      ORB.OpenScope; INC(level); type.base := ORB.noType;
+      ProcedureType(type, parblksize);  (*formal parameter list*)
+      type.dsc := ORB.topScope.next;
+      IF proc.type # type THEN  (*identifier found in the symbol table*)
+        IF (proc.class # ORB.Const) OR (proc.type.form # ORB.Proc) OR (proc.val >= 0) OR ~body THEN ORS.Mark("mult def")
+        ELSIF (proc.expo # expo) OR ~EqualSignatures(proc.type, type) THEN ORS.Mark("must match forward declaration")
+        END
+      END ;
+      IF body THEN Body(proc, parblksize, int) END ;
+      ORB.CloseScope; DEC(level)
     END
   END ProcedureDecl;
 
@@ -905,7 +918,7 @@
         Texts.WriteString(W, modid); Texts.Append(Oberon.Log, W.buf)
       ELSE ORS.Mark("identifier expected")
       END ;
-      Check(ORS.semicolon, "no ;"); level := 0; exno := 1; key := 0;
+      Check(ORS.semicolon, "no ;"); level := 0; fc := 0; exno := 1; key := 0;
       IF sym = ORS.import THEN
         ORS.Get(sym);
         WHILE sym = ORS.ident DO
@@ -926,6 +939,7 @@
       END ;
       ORG.Open(version); Declarations(dc); ORG.SetDataSize((dc + 3) DIV 4 * 4);
       WHILE sym = ORS.procedure DO ProcedureDecl; Check(ORS.semicolon, "no ;") END ;
+      IF fc > 0 THEN ORS.Mark("undefined forward declarations") END ;
       ORG.Header;
       IF sym = ORS.begin THEN ORS.Get(sym); StatSequence END ;
       Check(ORS.end, "no END");
```

# Appendix 2: Changes made to Project Oberon 2013 for use case B

**ORG.Mod**

Same as in use case A.

**ORP1.Mod**

```diff
--- FPGAOberon2013/ORP.Mod	2019-05-30 17:57:33.000000000 +0200
+++ Oberon-forward-references-of-procedures/Sources/FPGAOberon2013/ORP1.Mod	2019-11-09 14:15:31.000000000 +0100
@@ -1,4 +1,4 @@
-MODULE ORP; (*N. Wirth 1.7.97 / 31.5.2019  Oberon compiler for RISC in Oberon-07*)
+MODULE ORP; (*N. Wirth 1.7.97 / 17.9.2018 / AP 15.5.19  Oberon compiler for RISC in Oberon-07*)
   IMPORT Texts, Oberon, ORS, ORB, ORG;
   (*Author: Niklaus Wirth, 2014.
     Parser of Oberon-RISC compiler. Uses Scanner ORS to obtain symbols (tokens),
@@ -850,28 +850,25 @@
       type: ORB.Type;
       procid: ORS.Ident;
       x: ORG.Item;
-      locblksize, parblksize, L: LONGINT;
+      locblksize, parblksize: LONGINT;
       int: BOOLEAN;
-  BEGIN (* ProcedureDecl *) int := FALSE; ORS.Get(sym); 
+  BEGIN (* ProcedureDecl *) int := FALSE; ORS.Get(sym);
     IF sym = ORS.times THEN ORS.Get(sym); int := TRUE END ;
     IF sym = ORS.ident THEN
       ORS.CopyId(procid); ORS.Get(sym);
-      ORB.NewObj(proc, ORS.id, ORB.Const);
+      ORB.NewObj(proc, procid, ORB.Const);
       IF int THEN parblksize := 12 ELSE parblksize := 4 END ;
-      NEW(type); type.form := ORB.Proc; type.size := ORG.WordSize;
-      proc.type := type; proc.val := -1; proc.lev := level; 
+      NEW(type); type.form := ORB.Proc; type.size := ORG.WordSize; type.len := 0; (*heading of fixup list for forward refs*)
+      proc.type := type; proc.val := -1; proc.lev := level;
       CheckExport(proc.expo);
       IF proc.expo THEN proc.exno := exno; INC(exno) END ;
       ORB.OpenScope; INC(level); type.base := ORB.noType;
       ProcedureType(type, parblksize);  (*formal parameter list*)
-      Check(ORS.semicolon, "no ;"); locblksize := parblksize; 
-      Declarations(locblksize);
+      Check(ORS.semicolon, "no ;"); locblksize := parblksize;
+      Declarations(locblksize); proc.type.dsc := ORB.topScope.next;
+      WHILE sym = ORS.procedure DO ProcedureDecl; Check(ORS.semicolon, "no ;") END ;
+      ORG.FixLink(proc.type.len);  (*fix forward references generated in ORG.load and ORG.Call*)
       proc.val := ORG.Here() * 4; proc.type.dsc := ORB.topScope.next;
-      IF sym = ORS.procedure THEN
-        L := 0; ORG.FJump(L);
-        REPEAT ProcedureDecl; Check(ORS.semicolon, "no ;") UNTIL sym # ORS.procedure;
-        ORG.FixOne(L); proc.val := ORG.Here() * 4; proc.type.dsc := ORB.topScope.next
-      END ;
       ORG.Enter(parblksize, locblksize, int);
       IF sym = ORS.begin THEN ORS.Get(sym); StatSequence END ;
       IF sym = ORS.return THEN
@@ -882,13 +879,13 @@
       ELSIF type.base.form # ORB.NoTyp THEN
         ORS.Mark("function without result"); type.base := ORB.noType
       END ;
-      ORG.Return(type.base.form, x, locblksize, int);
-      ORB.CloseScope; DEC(level); Check(ORS.end, "no END");
+      ORG.Return(type.base.form, x, locblksize, int); Check(ORS.end, "no END");
       IF sym = ORS.ident THEN
         IF ORS.id # procid THEN ORS.Mark("no match") END ;
         ORS.Get(sym)
       ELSE ORS.Mark("no proc id")
-      END
+      END ;
+      ORB.CloseScope; DEC(level)
     END
   END ProcedureDecl;
```

# Appendix 3: Changes made to Extended Oberon for use case A

**ORB.Mod**

```diff
--- Oberon-extended/Sources/ORB.Mod  2020-04-18 13:39:57.000000000 +0200
+++ Oberon-forward-references-of-procedures/Sources/ExtendedOberon/ORB.Mod  2020-04-18 17:33:06.000000000 +0200
@@ -80,6 +80,13 @@
     END
   END NewObj;
 
+  PROCEDURE FindObj*(id: ORS.Ident; list: Object): Object;  (*search id in list*)
+    VAR x: Object;
+  BEGIN x := list;
+    WHILE (x # NIL) & (x.name # id) DO x := x.next END ;
+    RETURN x
+  END FindObj;
+
   PROCEDURE thisObj*(): Object;
     VAR s, x: Object;
   BEGIN s := topScope;
```

**ORP.Mod**

```diff
--- Oberon-extended/Sources/ORP.Mod  2020-04-18 17:36:17.000000000 +0200
+++ Oberon-forward-references-of-procedures/Sources/ExtendedOberon/ORP.Mod  2020-04-18 16:26:41.000000000 +0200
@@ -14,7 +14,7 @@
     END ;
   
   VAR sym: INTEGER;   (*last symbol read*)
-    dc: LONGINT;    (*data counter*)
+    dc, fc: LONGINT;    (*data counter, forward counter*)
     level, exno, version: INTEGER;
     newSF: BOOLEAN;  (*option flag*)
     expression: PROCEDURE (VAR x: ORG.Item);  (*to avoid forward reference*)
@@ -978,8 +978,8 @@
     VAR proc, redef, obj: ORB.Object;
       type, typ, rec: ORB.Type;
       procid, recid: ORS.Ident;
-      parblksize: LONGINT; class: INTEGER;
-      int, expo: BOOLEAN;
+      parblksize: LONGINT; form, class: INTEGER;
+      int, body, expo: BOOLEAN;
 
     PROCEDURE Body(proc: ORB.Object; parblksize: LONGINT; int: BOOLEAN);
       VAR x: ORG.Item; locblksize: LONGINT;
@@ -987,7 +987,7 @@
       Declarations(locblksize); proc.type.dsc := ORB.topScope.next;
       WHILE sym = ORS.procedure DO ProcedureDecl; Check(ORS.semicolon, "no ;") END ;
       ORG.FixLink(proc.type.len);  (*fix forward references generated in ORG*)
-      proc.val := ORG.Here() * 4; proc.type.dsc := ORB.topScope.next;
+      proc.val := ORG.Here() * 4; proc.type.dsc := ORB.topScope.next; DEC(fc);
       ORG.Enter(parblksize, locblksize, int);
       IF sym = ORS.begin THEN ORS.Get(sym); StatSequence END ;
       IF sym = ORS.return THEN
@@ -1006,30 +1006,43 @@
       END
     END Body;
 
-  BEGIN (* ProcedureDecl *) int := FALSE; rec := NIL; ORS.Get(sym);
-    IF sym = ORS.times THEN ORS.Get(sym); int := TRUE END ;
+  BEGIN (* ProcedureDecl *) int := FALSE; body := TRUE; rec := NIL; ORS.Get(sym);
+    IF sym = ORS.times THEN (*interrupt*) ORS.Get(sym); int := TRUE
+    ELSIF sym = ORS.arrow THEN (*forward*) ORS.Get(sym); body := FALSE
+    END ;
     IF sym = ORS.lparen THEN
-      ORS.Get(sym); Receiver(class, recid, typ, rec);
+      ORS.Get(sym); Receiver(class, recid, typ, rec); form := ORB.TProc;
       IF level # 0 THEN ORS.Mark("local type-bound procedures not implemented") END
+    ELSE form := ORB.Proc
     END ;
     IF sym = ORS.ident THEN
       ORS.CopyId(procid); ORS.Get(sym); CheckExport(expo);
       IF int THEN parblksize := 12 ELSE parblksize := 4 END ;
       NEW(type); type.size := ORG.WordSize; type.len := 0; (*len used as heading of fixup chain of forward refs*)
       IF rec = NIL THEN  (*regular procedure*)
-        ORB.NewObj(proc, procid, ORB.Const);
-        type.form := ORB.Proc; proc.type := type; proc.val := -1; proc.lev := level; proc.expo := expo;
-        IF expo THEN proc.exno := exno; INC(exno) END ;
+        proc := ORB.FindObj(procid, ORB.topScope.next);
+        IF proc = NIL THEN  (*identifier not found in the symbol table*)
+          ORB.NewObj(proc, procid, ORB.Const); INC(fc);
+          type.form := ORB.Proc; proc.type := type; proc.val := -1; proc.lev := level; proc.expo := expo;
+          IF expo THEN proc.exno := exno; INC(exno) END
+        END ;
         ORB.OpenScope; INC(level); type.base := ORB.noType;
         ProcedureType(type, parblksize); type.dsc := ORB.topScope.next  (*formal parameter list*)
       ELSE  (*type-bound procedure*)
-        ORB.NewMethod(rec, proc, redef, procid);
-        IF rec.typobj.val > 0 THEN ORS.Mark("invalid method order") ELSE DisallowMethods(rec.base) END ;
-        type.form := ORB.TProc; proc.type := type; proc.val := -1; proc.expo := expo;
-        IF expo THEN proc.exno := exno; INC(exno);
-          IF ~typ.typobj.expo THEN ORS.Mark("receiver must be exported") END ;
-          procid := "@"; ORB.NewObj(obj, procid, ORB.Const); obj.name[0] := 0X; (*dummy to preserve linear order of exno*)
-          obj.type := proc.type; obj.dsc := proc; obj.exno := proc.exno; obj.expo := FALSE
+        IF rec.base # NIL THEN redef := ORB.FindObj(procid, rec.base.dsc);  (*search in base types of received*)
+          IF (redef # NIL) & ((redef.class # ORB.Const) OR (redef.type.form # ORB.TProc)) THEN ORS.Mark("mult def") END
+        ELSE redef := NIL
+        END ;
+        proc := ORB.FindFld(procid, rec);  (*search in fields of receiver proper, but not of its base types*)
+        IF proc = NIL THEN
+          ORB.NewMethod(rec, proc, redef, procid); INC(fc);
+          IF rec.typobj.val > 0 THEN ORS.Mark("invalid method order") ELSE DisallowMethods(rec.base) END ;
+          type.form := ORB.TProc; proc.type := type; proc.val := -1; proc.expo := expo;
+          IF expo THEN proc.exno := exno; INC(exno);
+            IF ~typ.typobj.expo THEN ORS.Mark("receiver must be exported") END ;
+            procid := "@"; ORB.NewObj(obj, procid, ORB.Const); obj.name[0] := 0X; (*dummy to preserve linear order of exno*)
+            obj.type := proc.type; obj.dsc := proc; obj.exno := proc.exno; obj.expo := FALSE
+          END
         END ;
         ORB.OpenScope; INC(level); type.base := ORB.noType;
         ORB.NewObj(obj, recid, class);  (*insert receiver as first parameter*)
@@ -1042,7 +1055,13 @@
           END
         END
       END ;
-      Body(proc, parblksize, int); ORB.CloseScope; DEC(level)
+      IF proc.type # type THEN  (*identifier found in the symbol table*)
+        IF (proc.class # ORB.Const) OR (proc.type.form # form) OR (proc.val >= 0) OR ~body THEN ORS.Mark("mult def")
+        ELSIF (proc.expo # expo) OR ~EqualSignatures(proc.type, type) THEN ORS.Mark("must match forward declaration")
+        END
+      END ;
+      IF body THEN Body(proc, parblksize, int) END ;
+      ORB.CloseScope; DEC(level)
     ELSE ORS.Mark("proc id expected")
     END
   END ProcedureDecl;
@@ -1078,10 +1097,11 @@
         Texts.WriteString(W, modid); Texts.Append(Oberon.Log, W.buf)
       ELSE ORS.Mark("identifier expected")
       END ;
-      Check(ORS.semicolon, "no ;"); level := 0; exno := 1; key := 0;
+      Check(ORS.semicolon, "no ;"); level := 0; fc := 0; exno := 1; key := 0;
       IF sym = ORS.import THEN ImportList; Check(ORS.semicolon, "; missing") END ;
       ORG.Open(version); Declarations(dc); ORG.SetDataSize((dc + 3) DIV 4 * 4);
       WHILE sym = ORS.procedure DO ProcedureDecl; Check(ORS.semicolon, "no ;") END ;
+      IF fc > 0 THEN ORS.Mark("undefined forward declarations") END ;
       ORG.Header;
       IF sym = ORS.begin THEN ORS.Get(sym); StatSequence END ;
       Check(ORS.end, "no END");
```

# Appendix 4: Changes made to Extended Oberon for use case B

Use case B is the default implementation in Extended Oberon.

