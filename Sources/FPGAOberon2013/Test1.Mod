MODULE Test1; (*test forward calls in nested procedures*)
  IMPORT Texts, Oberon;
  VAR W: Texts.Writer;

  PROCEDURE Go*;

  PROCEDURE P(i: INTEGER);

    PROCEDURE Q(i: INTEGER);
    BEGIN Texts.Write(W, "Q"); Texts.WriteInt(W, i, 4); Texts.WriteLn(W); Texts.Append(Oberon.Log, W.buf);
      IF i > 0 THEN P(i-1) END
    END Q;

  BEGIN Texts.Write(W, "P"); Texts.WriteInt(W, i, 4); Texts.WriteLn(W); Texts.Append(Oberon.Log, W.buf);
     Q(i-1)
  END P;

  BEGIN P(7)
  END Go;

BEGIN Texts.OpenWriter(W)
END Test1.

ORP.Compile ORB1.Mod/s ORG1.Mod/s ORP1.Mod ~   # compile the new compiler
System.Free ORTool ORP ORG ORB ORS ~           # unload the old compiler

ORP.Compile Test1.Mod ~
System.Free Test1 ~
ORTool.DecObj Test1.rsc ~

Test1.Go
