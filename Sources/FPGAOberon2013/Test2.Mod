MODULE Test2; (*test forward declarations of procedures*)
  IMPORT Texts, Oberon;
  TYPE T = PROCEDURE(VAR x, y: INTEGER; z: REAL);
  VAR W: Texts.Writer; G: T;

  PROCEDURE^ P(VAR x, y: INTEGER; z: REAL);

  PROCEDURE w(s: ARRAY OF CHAR);
  BEGIN Texts.WriteString(W, s); Texts.WriteLn(W); Texts.Append(Oberon.Log, W.buf)
  END w;

  PROCEDURE A(VAR x, y: INTEGER; z: REAL); BEGIN w("A") END A;

  PROCEDURE Go1*;
    VAR i, j: INTEGER; k: REAL; b: T;

    PROCEDURE S(t: T): T;
      VAR a, b: INTEGER; c: REAL;
    BEGIN w("S"); A(a, b, c); P(a, b, c); RETURN t
    END S;

  BEGIN w("Go1");
    A(i, j, k);
    P(i, j, k);
    b := S(P); b(i, j, k);
    b := S(A); b(i, j, k);
    G := A; G(i, j, k);
    G := P; G(i, j, k)
  END Go1;

  PROCEDURE P(VAR x, y: INTEGER; z: REAL);
    PROCEDURE Q;
      PROCEDURE R;
        VAR a, b: INTEGER; c: REAL;
      BEGIN w("R"); A(a, b, c)
      END R;
    BEGIN w("Q"); R
    END Q;
  BEGIN w("P"); Q
  END P;

  PROCEDURE Go2*;
    VAR i, j: INTEGER; k: REAL;
  BEGIN w("Go2"); G := P; G(i, j, k)
  END Go2;

BEGIN Texts.OpenWriter(W); G := A
END Test2.

ORP.Compile ORB1.Mod/s ORG1.Mod/s ORP2.Mod ~   # compile the new compiler
System.Free ORTool ORP ORG ORB ORS ~           # unload the old compiler

ORP.Compile Test2.Mod/s ~
System.Free Test2 ~
ORTool.DecObj Test2.rsc ~

Test2.Go1
Test2.Go2
