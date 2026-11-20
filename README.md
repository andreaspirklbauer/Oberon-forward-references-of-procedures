# Oberon-forward-references-of-procedures
Modified Oberon-07 compiler that implements forward references of procedures for 2 use cases:

* **Use case A:** To make *references* among nested procedures more efficient
* **Use case B (includes use case A):** To implement forward *declarations* of procedures

Note: In this repository, the term "Project Oberon 2013" refers to a re-implementation of the original "Project Oberon" on an FPGA development board around 2013, as published at www.projectoberon.com.

**PREREQUISITES**: A current version of Project Oberon 2013 (see http://www.projectoberon.com) or Extended Oberon (see http://github.com/andreaspirklbauer/Oberon-extended).

------------------------------------------------------
**Use case A:** To make *references* among nested procedures more efficient, use source files *ORB1.Mod*, *ORG1.Mod* and *ORP1.Mod*.

This implements only forward *references*, but not forward *declarations* of procedures.

If a procedure Q which is local to procedure P refers to the enclosing procedure P, as in

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
**Use case B (includes use case A):** To implement both forward *references* of procedures **and** forward *declarations* of procedures, use source files *ORP2.Mod* instead of *ORP1.Mod* (the other source files being the same as in use case A):

This adds forward *declarations* of procedures (in the source language) to use case A. Note that with the changes needed for use case A, the implementation of forward *declarations* of procedures comes practically for free.

Forward declarations of procedures have been eliminated in the Oberon-07 language revision of 2013/2016 (see http://inf.ethz.ch/personal/wirth/Oberon/Oberon07.Report.pdf).

The rationale was that forward declarations of procedures can always be eliminated from any program by an appropriate nesting or by introducing procedure variables (see chapter 2 of www.inf.ethz.ch/personal/wirth/Oberon/PortingOberon.pdf).

Another argument for eliminating forward declaration of procedures is that the language already has the language construct of *procedure variables*, which are needed for handlers and call backs. Therefore, one doesn‘t *also* need the additional language construct of *forward declarations* of procedures.

Here, we have re-introduced them for three main reasons:

1. Backward-compatibility: Legacy programs that contain forward references of procedures are now accepted again by the compiler.
2. Direct procedure calls are more efficient than using procedure variables.
3. Introducing forward declarations of procedures added only about 10 lines of source code (relative to use case 1).

Forward declarations of procedures are implemented in the same way as in the original implementation before the Oberon-07 language revision, i.e.

1. They are explicitly specified by ^ following the symbol PROCEDURE in the source text.
2. The compiler processes the heading in the normal way, assuming its body to be missing. The newly generated object in the symbol table is marked as a forward declaration.
3. When later in the source text the full declaration is encountered, the symbol table is first searched. If the given identifier is found and denotes a procedure, the full declaration is associated with the already existing entry in the symbol table and the parameter lists are compared. Otherwise a multiple definition of the same identifier is present.

------------------------------------------------------
**Implementation**

The modified compiler implements forward *declarations* of procedures as described below:

Note that in our implementation both global *and* local procedures can be declared forward.

**1. Processing of the procedure heading of a forward-declared procedure P (ORP.ProcedureDecl)**:

When this *heading* of a forward-declared procedure P, i.e. the heading

      PROCEDURE^ P(x: INTEGER);

is processed, the field *obj.type.len* is set to 0 to indicate that no forward reference to P has been generated yet, and *obj.val* is set to -1 to indicate that the body of P has not been compiled yet. See *ORP.ProcedureDecl*:

        proc.val := -1;       (*<0: body of P has not been compiled yet; otherwise: entry address of P*)

The field *proc.type.len* is used as the heading of the fixup list for forward references to P (initially set to 0). This is acceptable, because every procedure object *obj* (of type *ORB.Object*) has its **own** type object *obj.type* (of type *ORB.Type*) and its field *obj.type.len* is not used otherwise.

The field *obj.type.len* is available during code generation as the field *x.type.len* in source level items generated from the procedure object *obj* using procedure *ORG.MakeItem*, while the field *obj.val* is available as the field *x.a*.

**2. Assigning P to a procedure variable, passing P as a procedure parameter, returning P as a result of a function procedure (ORG.load)**:

If a procedure P, whose body has not been compiled yet, is assigned to a procedure variable, passed as parameter to a procedure or returned as the result of a function procedure, a *forward reference* in the form of a *register* instruction is generated that will eventually contain an instruction operand.

This adds a single line to *ORG.load*:

       IF x.a < 0 THEN (*forward*) Put3(BL, 7, 0); Put1(Add, RH, LNK, x.type.len); x.type.len := pc-1    (*fixed up in ORP.Body*)

The purpose of the first instruction generated (branch zero step forward) is to merely deposit the link address PC+1 in register LNK ("LNK := PC+1"). To this address, we then add the code distance to procedure P (determined later during the fixup step in ORP.Body).

Here we use PC-relative addressing, so that we can fix up this instruction at *compile* time rather than only at module *load* time.

**3. Calling a forward-declared procedure P (ORG.Call)**:

If a procedure P, whose body has not been compiled yet, is *called*, a forward reference in the form of a *branch* instruction is generated that will eventually contain the branch displacement.

This adds a single line to *ORG.Call*:.

       IF x.a < 0 THEN (*forward*) Put3(BL, 7, x.type.len); x.type.len := pc-1  (*fixed up in ORP.Body*)

**4. Compilation of the procedure body of a forward-declared procedure P (ORP.Body)**:

When the procedure *body* of P is *compiled*, all forward references to P are *fixed up* with the now known actual entry address of P, and the field *obj.val* is changed to that address.

   This changes *ORP.ProcedureDecl* from:

        proc.val := ORG.Here() * 4; proc.type.dsc := ORB.topScope.next;
        IF sym = ORS.procedure THEN
          L := 0; ORG.FJump(L);
          REPEAT ProcedureDecl; Check(ORS.semicolon, "no ;") UNTIL sym # ORS.procedure;
          ORG.FixOne(L); proc.val := ORG.Here() * 4; proc.type.dsc := ORB.topScope.next
        END ;

   to the following code excerpt (in a newly introduced local procedure *ORP.Body*):

        proc.type.dsc := ORB.topScope.next;
        WHILE sym = ORS.procedure DO ProcedureDecl; Check(ORS.semicolon, "no ;") END ;
        ORG.FixLinkMixed(proc.type.len);  (*fix forward references generated in ORG.load and ORG.Call*)
        proc.val := ORG.Here() * 4; proc.type.dsc := ORB.topScope.next; DEC(fc);

As one can see, the forward jump at the beginning of P is no longer generated (no calls to *ORG.FJump* anymore).

The second assignment *proc.type.dsc := ORB.topScope.next* is necessary to cover the case, where *proc.type.dsc* has been NIL before local procedures have been processed (in this case, a new object for the local procedure will be added).

Note that procedure *ORG.FixLink* of the Project Oberon 2013 compiler assumes that the instructions to be fixed up are format-3 *branch* instructions. But in our implementation we also generate format-1 *register* instructions (in *ORG.load*, see above).

We could have decided to just generalize *ORG.FixLink* to also handle the format-1 ADD instruction generated in *ORG.load*.

However, we have opted to add a *separate* procedure *ORG.FixLinkMixed* that can handle both format-1 and format-3 instructions embedded in the *same* fixup list.

This has the advantage, that the (more complex) code in *ORG.FixLinkMixed* is only called when compiling procedure bodies, and not in all cases where format-3 branch instructions are to be fixed up. This choice also nicely isolates the addition of this feature, which, after all, exists only in the retro compiler, and only to implement forward declarations of procedures.

        PROCEDURE fix1(at, with: LONGINT);
          VAR v: LONGINT;
        BEGIN (*fix format-1 register instruction*)
           IF with < 0 THEN v := C28 (*set v bit*) ELSE v := 0 END ;
           code[at] := code[at] DIV C16 * C16 + with MOD C16 + v
        END fix1;

        PROCEDURE FixLinkMixed*(L: LONGINT);
          VAR L1, format: LONGINT; p: INTEGER;
        BEGIN (*fix chain of instructions of different formats*)
          WHILE L # 0 DO p := code[L];
            format := p DIV C30 MOD 4; L1 := p MOD C16;
            IF format < 3 THEN fix1(L, (pc-L)*4) ELSE fix(L, pc-L-1) END ;
            L := L1
          END
        END FixLinkMixed;

Note that it is *essential* that the origin of the fixup list for a procedure P is **not** rooted in a variable *L* local to the parsing procedure *ProcedureDecl* (as is typical in other parsing procedures), because forward references may be generated from *other* procedures in

* the surrounding scope,
* the same scope, or
* from within a nested scope (as in the example above).

However, the fixup list must be associated with P at all times. A field in the symbol table entry (such as *obj.type.len*) for P is therefore ideally suited for this purpose.

**5. Compilations of calls to P after the procedure body of P has been compiled**:

Any references to P later in the source text are *backward* references using the actual entry address of P, and no fixups are needed for such calls.

------------------------------------------------------
**Preparing your compiler to support forward references of procedures**

Convert the downloaded files to Oberon format (Oberon uses CR as line endings) using the command [**dos2oberon**](dos2oberon), also available in this repository (example shown for Mac or Linux):

     for x in *.Mod ; do ./dos2oberon $x $x ; done

Import the files to your Oberon system. If you use an emulator (e.g., **https://github.com/pdewacht/oberon-risc-emu**) to run the Oberon system, click on the *PCLink1.Run* link in the *System.Tool* viewer, copy the files to the emulator directory, and execute the following command on the command shell of your host system:

     cd oberon-risc-emu
     for x in *.Mod ; do ./pcreceive.sh $x ; sleep 0.5 ; done

Rebuild the Oberon compiler, depending on the use case that you want to implement

Use case A (see Test1.Mod):

     ORP.Compile ORB1.Mod/s ORG1.Mod/s ORP1.Mod ~     # build a compiler for use case A

Use case B (see Test2.Mod):

     ORP.Compile ORB1.Mod/s ORG1.Mod/s ORP2.Mod ~     # build a compiler for use case B

Followed by:

     System.Free ORTool ORP ORG ORB ORS ~          # unload the old compiler used to build the new one

See the test programs *Test.Mod* and *Test1.Mod* for the correct compilation order.

# Appendix 1: Changes made to Project Oberon 2013 for use case A

**ORB1.Mod**

```diff
--- FPGAOberon2013/ORB.Mod	2020-03-06 13:20:41.000000000 +0100
+++ Oberon-forward-references-of-procedures/Sources/FPGAOberon2013/ORB1.Mod	2021-10-23 12:27:24.000000000 +0200
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

**ORG1.Mod**

```diff
--- FPGAOberon2013/ORG.Mod  2019-05-30 17:58:14.000000000 +0200
+++ Oberon-forward-references-of-procedures/Sources/FPGAOberon2013/ORG1.Mod  2021-10-23 18:22:30.000000000 +0200
@@ -7,8 +7,10 @@
   CONST WordSize* = 4;
     StkOrg0 = -64; VarOrg0 = 0;  (*for RISC-0 only*)
     MT = 12; SP = 14; LNK = 15;   (*dedicated registers*)
-    maxCode = 8000; maxStrx = 2400; maxTD = 160; C24 = 1000000H;
+    maxCode = 8000; maxStrx = 3000; maxTD = 160;
     Reg = 10; RegI = 11; Cond = 12;  (*internal item modes*)
+    C4 = 10H; C6 = 40H; C8 = 100H; C10 = 400H; C12 = 1000H; C14 = 4000H; C16 = 10000H; C18 = 40000H;
+    C20 = 100000H; C22 = 400000H; C24 = 1000000H; C26 = 4000000H; C28 = 10000000H; C30 = 40000000H;
 
   (*frequently used opcodes*)  U = 2000H; V = 1000H;
     Mov = 0; Lsl = 1; Asr = 2; Ror= 3; And = 4; Ann = 5; Ior = 6; Xor = 7;
@@ -109,14 +111,17 @@
     RETURN cond
   END negated;
 
+  PROCEDURE fix1(at, with: LONGINT);
+    VAR v: LONGINT;
+  BEGIN (*fix format-1 register instruction*)
+    IF with < 0 THEN v := C28 (*set v bit*) ELSE v := 0 END ;
+    code[at] := code[at] DIV C16 * C16 + with MOD C16 + v
+  END fix1;
+
   PROCEDURE fix(at, with: LONGINT);
   BEGIN code[at] := code[at] DIV C24 * C24 + (with MOD C24)
   END fix;
 
-  PROCEDURE FixOne*(at: LONGINT);
-  BEGIN fix(at, pc-at-1)
-  END FixOne;
-
   PROCEDURE FixLink*(L: LONGINT);
     VAR L1: LONGINT;
   BEGIN
@@ -132,6 +137,16 @@
     END
   END FixLinkWith;
 
+  PROCEDURE FixLinkMixed*(L: LONGINT);
+    VAR L1, format: LONGINT; p: INTEGER;
+  BEGIN (*fix chain of instructions of different formats*)
+    WHILE L # 0 DO p := code[L];
+      format := p DIV C30 MOD 4; L1 := p MOD C16;
+      IF format < 3 THEN fix1(L, (pc-L)*4) ELSE fix(L, pc-L-1) END ;
+      L := L1
+    END
+  END FixLinkMixed;
+
   PROCEDURE merged(L0, L1: LONGINT): LONGINT;
     VAR L2, L3: LONGINT;
   BEGIN 
@@ -162,7 +177,8 @@
     IF x.mode # Reg THEN
       IF x.mode = ORB.Const THEN
         IF x.type.form = ORB.Proc THEN
-          IF x.r > 0 THEN ORS.Mark("not allowed")
+          IF x.a < 0 THEN (*forward*) Put3(BL, 7, 0); Put1(Add, RH, LNK, x.type.len); x.type.len := pc-1  (*fixed up in ORP.Body*)
+          ELSIF x.r > 0 THEN ORS.Mark("not allowed")
           ELSIF x.r = 0 THEN Put3(BL, 7, 0); Put1a(Sub, RH, LNK, pc*4 - x.a)
           ELSE GetSB(x.r); Put1(Add, RH, RH, x.a + 100H) (*mark as progbase-relative*)
           END
@@ -771,7 +787,8 @@
   PROCEDURE Call*(VAR x: Item; r: LONGINT);
   BEGIN (*x.type.form = ORB.Proc*)
     IF x.mode = ORB.Const THEN
-      IF x.r >= 0 THEN Put3(BL, 7, (x.a DIV 4)-pc-1)
+      IF x.a < 0 THEN (*forward*) Put3(BL, 7, x.type.len); x.type.len := pc-1  (*fixed up in ORP.Body*)
+      ELSIF x.r >= 0 THEN Put3(BL, 7, (x.a DIV 4)-pc-1)
       ELSE (*imported*)
         IF pc - fixorgP < 1000H THEN
           Put3(BL, 7, ((-x.r) * 100H + x.a) * 1000H + pc-fixorgP); fixorgP := pc-1
```

**ORP1.Mod**

```diff
--- FPGAOberon2013/ORP.Mod  2021-05-24 10:06:15.000000000 +0200
+++ Oberon-forward-references-of-procedures/Sources/FPGAOberon2013/ORP1.Mod  2021-10-23 18:15:11.000000000 +0200
@@ -850,7 +850,7 @@
       type: ORB.Type;
       procid: ORS.Ident;
       x: ORG.Item;
-      locblksize, parblksize, L: LONGINT;
+      locblksize, parblksize: LONGINT;
       int: BOOLEAN;
   BEGIN (* ProcedureDecl *) int := FALSE; ORS.Get(sym); 
     IF sym = ORS.times THEN ORS.Get(sym); int := TRUE END ;
@@ -865,13 +865,10 @@
       ORB.OpenScope; INC(level); type.base := ORB.noType;
       ProcedureType(type, parblksize);  (*formal parameter list*)
       Check(ORS.semicolon, "no ;"); locblksize := parblksize; 
-      Declarations(locblksize);
+      Declarations(locblksize); proc.type.dsc := ORB.topScope.next;
+      WHILE sym = ORS.procedure DO ProcedureDecl; Check(ORS.semicolon, "no ;") END ;
+      ORG.FixLinkMixed(proc.type.len);  (*fix forward references generated in ORG*)
       proc.val := ORG.Here() * 4; proc.type.dsc := ORB.topScope.next;
-      IF sym = ORS.procedure THEN
-        L := 0; ORG.FJump(L);
-        REPEAT ProcedureDecl; Check(ORS.semicolon, "no ;") UNTIL sym # ORS.procedure;
-        ORG.FixOne(L); proc.val := ORG.Here() * 4; proc.type.dsc := ORB.topScope.next
-      END ;
       ORG.Enter(parblksize, locblksize, int);
       IF sym = ORS.begin THEN ORS.Get(sym); StatSequence END ;
       IF sym = ORS.return THEN
```

# Appendix 2: Changes made to Project Oberon 2013 for use case B

**ORB1.Mod**

Same as in use case A

**ORG1.Mod**

Same as in use case A

**ORP2.Mod**

```diff
--- FPGAOberon2013/ORP.Mod  2021-05-24 10:06:15.000000000 +0200
+++ Oberon-forward-references-of-procedures/Sources/FPGAOberon2013/ORP2.Mod  2021-10-23 18:15:15.000000000 +0200
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
+      ORG.FixLinkMixed(proc.type.len);  (*fix forward references generated in ORG*)
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
+      NEW(type); type.form := ORB.Proc; type.size := ORG.WordSize;
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
     ELSE ORS.Mark("proc id expected")
     END
   END ProcedureDecl;
@@ -922,7 +935,7 @@
         Texts.WriteString(W, modid); Texts.Append(Oberon.Log, W.buf)
       ELSE ORS.Mark("identifier expected")
       END ;
-      Check(ORS.semicolon, "no ;"); level := 0; exno := 1; key := 0;
+      Check(ORS.semicolon, "no ;"); level := 0; fc := 0; exno := 1; key := 0;
       IF sym = ORS.import THEN
         ORS.Get(sym); Import;
         WHILE sym = ORS.comma DO ORS.Get(sym); Import END ;
@@ -930,6 +943,7 @@
       END ;
       ORG.Open(version); Declarations(dc); ORG.SetDataSize((dc + 3) DIV 4 * 4);
       WHILE sym = ORS.procedure DO ProcedureDecl; Check(ORS.semicolon, "no ;") END ;
+      IF fc > 0 THEN ORS.Mark("undefined forward declarations") END ;
       ORG.Header;
       IF sym = ORS.begin THEN ORS.Get(sym); StatSequence END ;
       Check(ORS.end, "no END");
```

# Appendix 3: Changes made to Extended Oberon for use case A

**ORB1.Mod**

```diff
82a83,89
>   PROCEDURE FindObj*(id: ORS.Ident; list: Object): Object;  (*search id in list*)
>     VAR x: Object;
>   BEGIN x := list;
>     WHILE (x # NIL) & (x.name # id) DO x := x.next END ;
>     RETURN x
>   END FindObj;
> 
```

**ORG1.Mod**

```diff
--- Oberon-extended/Sources/ORG.Mod  2021-10-23 17:26:18.000000000 +0200
+++ Oberon-forward-references-of-procedures/Sources/ExtendedOberon/ORG1.Mod  2021-10-23 18:08:23.000000000 +0200
@@ -160,10 +160,6 @@
     code[at] := code[at] DIV C24 * C24 + with MOD C24
   END fix3;
 
-  PROCEDURE FixOne*(at: LONGINT);
-  BEGIN fix3(at, pc-at-1)
-  END FixOne;
-
   PROCEDURE FixLinkWith(L, dst: LONGINT);
     VAR L1: LONGINT;
   BEGIN (*fix chain of branch instructions*)
@@ -174,6 +170,16 @@
   BEGIN FixLinkWith(L, pc)
   END FixLink;
 
+  PROCEDURE FixLinkMixed*(L: LONGINT);
+    VAR L1, format: LONGINT; p: INTEGER;
+  BEGIN (*fix chain of instructions of different formats*)
+    WHILE L # 0 DO p := code[L];
+      format := p DIV C30 MOD 4; L1 := p MOD C16;
+      IF format < 3 THEN fix1(L, (pc-L)*4) ELSE fix3(L, pc-L-1) END ;
+      L := L1
+    END
+  END FixLinkMixed;
+
   PROCEDURE FixLinkPair(L, adr: LONGINT);
     VAR L1: LONGINT; p, q: INTEGER;
   BEGIN (*fix chain of instruction pairs with an address that is spread across both instructions, 0 <= adr < C24*)
@@ -204,7 +210,8 @@
       IF x.type.size = 1 THEN op := Ldr+1 ELSE op := Ldr END ;
       IF x.mode = ORB.Const THEN
         IF x.type.form = ORB.Proc THEN
-          IF x.r > 0 THEN (*local*) ORS.Mark("not allowed")
+          IF x.a < 0 THEN (*forward*) Put3(BL, 7, 0); Put1(Add, RH, LNK, x.type.len); x.type.len := pc-1  (*fixed up in ORP.Body*)
+          ELSIF x.r > 0 THEN (*local*) ORS.Mark("not allowed")
           ELSIF x.r = 0 THEN (*global*) Put3(BL, 7, 0); Put1a(Sub, RH, LNK, pc*4 - x.a)
           ELSE (*imported*) PutPair(x.r, Add, RH, RH, x.a + C8, 1) (*mark as progbase-relative*)
           END
@@ -849,7 +856,8 @@
         Put2(Ldr, RH, RH, -4-x.a*4); Put3(BLR, 7, RH)
       END
     ELSIF x.mode = ORB.Const THEN  (*regular procedure*)
-      IF x.r >= 0 THEN Put3(BL, 7, (x.a DIV 4)-pc-1)
+      IF x.a < 0 THEN (*forward*) Put3(BL, 7, x.type.len); x.type.len := pc-1  (*fixed up in ORP.Body*)
+      ELSIF x.r >= 0 THEN Put3(BL, 7, (x.a DIV 4)-pc-1)
       ELSE (*imported*) Put3a(BL, -x.r, x.a, pc-fixorgP); fixorgP := pc-1
       END
     ELSE  (*installed procedure*)
```

**ORP1.Mod**

```diff
--- Oberon-extended/Sources/ORP.Mod  2021-10-23 09:56:42.000000000 +0200
+++ Oberon-forward-references-of-procedures/Sources/ExtendedOberon/ORP1.Mod  2021-10-23 17:59:51.000000000 +0200
@@ -989,15 +989,12 @@
       int, expo: BOOLEAN;
 
     PROCEDURE Body(proc: ORB.Object; parblksize: LONGINT; int: BOOLEAN);
-      VAR x: ORG.Item; locblksize, L: LONGINT;
+      VAR x: ORG.Item; locblksize: LONGINT;
     BEGIN Check(ORS.semicolon, "no ;"); locblksize := parblksize;
-      Declarations(locblksize);
+      Declarations(locblksize); proc.type.dsc := ORB.topScope.next;
+      WHILE sym = ORS.procedure DO ProcedureDecl; Check(ORS.semicolon, "no ;") END ;
+      ORG.FixLinkMixed(proc.type.len);  (*fix forward references generated in ORG*)
       proc.val := ORG.Here() * 4; proc.type.dsc := ORB.topScope.next;
-      IF sym = ORS.procedure THEN
-        L := 0; ORG.FJump(L);
-        REPEAT ProcedureDecl; Check(ORS.semicolon, "no ;") UNTIL sym # ORS.procedure;
-        ORG.FixOne(L); proc.val := ORG.Here() * 4; proc.type.dsc := ORB.topScope.next
-      END ;
       ORG.Enter(parblksize, locblksize, int);
       IF sym = ORS.begin THEN ORS.Get(sym); StatSequence END ;
       IF sym = ORS.return THEN
```

# Appendix 4: Changes made to Extended Oberon for use case B

**ORB1.Mod**

Same as in use case A

**ORG1.Mod**

Same as in use case A

**ORP2.Mod**

```diff
--- Oberon-extended/Sources/ORP.Mod  2021-10-23 09:56:42.000000000 +0200
+++ Oberon-forward-references-of-procedures/Sources/ExtendedOberon/ORP2.Mod  2021-10-23 17:59:57.000000000 +0200
@@ -14,7 +14,7 @@
     END ;
   
   VAR sym: INTEGER;   (*last symbol read*)
-    dc: LONGINT;    (*data counter*)
+    dc, fc: LONGINT;    (*data counter, forward counter*)
     level, exno, version: INTEGER;
     newSF: BOOLEAN;  (*option flag*)
     expression: PROCEDURE (VAR x: ORG.Item);  (*to avoid forward reference*)
@@ -985,19 +985,16 @@
     VAR proc, redef, obj: ORB.Object;
       type, typ, rec: ORB.Type;
       procid, recid: ORS.Ident;
-      parblksize: LONGINT; class: INTEGER;
-      int, expo: BOOLEAN;
+      parblksize: LONGINT; form, class: INTEGER;
+      int, body, expo: BOOLEAN;
 
     PROCEDURE Body(proc: ORB.Object; parblksize: LONGINT; int: BOOLEAN);
-      VAR x: ORG.Item; locblksize, L: LONGINT;
+      VAR x: ORG.Item; locblksize: LONGINT;
     BEGIN Check(ORS.semicolon, "no ;"); locblksize := parblksize;
-      Declarations(locblksize);
-      proc.val := ORG.Here() * 4; proc.type.dsc := ORB.topScope.next;
-      IF sym = ORS.procedure THEN
-        L := 0; ORG.FJump(L);
-        REPEAT ProcedureDecl; Check(ORS.semicolon, "no ;") UNTIL sym # ORS.procedure;
-        ORG.FixOne(L); proc.val := ORG.Here() * 4; proc.type.dsc := ORB.topScope.next
-      END ;
+      Declarations(locblksize); proc.type.dsc := ORB.topScope.next;
+      WHILE sym = ORS.procedure DO ProcedureDecl; Check(ORS.semicolon, "no ;") END ;
+      ORG.FixLinkMixed(proc.type.len);  (*fix forward references generated in ORG*)
+      proc.val := ORG.Here() * 4; proc.type.dsc := ORB.topScope.next; DEC(fc);
       ORG.Enter(parblksize, locblksize, int);
       IF sym = ORS.begin THEN ORS.Get(sym); StatSequence END ;
       IF sym = ORS.return THEN
@@ -1016,30 +1013,43 @@
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
       NEW(type); type.size := ORG.WordSize;
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
+        IF rec.base # NIL THEN redef := ORB.FindObj(procid, rec.base.dsc);  (*search in base types of receiver*)
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
@@ -1052,7 +1062,13 @@
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
@@ -1088,10 +1104,11 @@
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
       ORG.Exit;
```



