unit ModifyedSleUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;
type
ArbInt=Integer;
ArbFloat=Double;
arfloat1   = array[1..20000] of ArbFloat;
 Procedure slegen(n, rwidth: ArbInt; Var a, b, x, ca: ArbFloat;
                 Var term: ArbInt);
 const
   highestelement=20000;
implementation
Function omvn1v(Var a: ArbFloat; n: ArbInt): ArbFloat;

Var   pa : ^arfloat1;
       i : ArbInt;
    norm : ArbFloat;

Begin
  If n<1 Then
    exit;
  pa := @a;
  norm := 0;
  For i:=1 To n Do
    norm := norm+abs(pa^[i]);
  omvn1v := norm
End {omvn1v};
Procedure slegen(n, rwidth: ArbInt; Var a, b, x, ca: ArbFloat;
                 Var term: ArbInt);

Var
          nsr, i, j, k, ipiv, ip, ik, i1n, k1n : ArbInt;
                                      singular : boolean;
           normr, pivot, l, normt, maxim, h, s : ArbFloat;
                pa, px, pb, au, sumrow, t, row : ^arfloat1;

Begin
  If (n<1) Or (rwidth<1)
   Then
    Begin
      term := 3;
     exit
    End; {wrong input}
  getmem(au, sqr(n)*sizeof(ArbFloat));
  nsr := n*sizeof(ArbFloat);
  getmem(t, nsr);
  getmem(row, nsr);
  getmem(sumrow, nsr);
  pa := @a;
 pb := @b;
 px := @x;
  For i:= 1 To n Do
    move(pa^[1+(i-1)*rwidth], au^[1+(i-1)*n], nsr);
  move(pb^[1], px^[1], nsr);
  normr := 0;
 singular := false ;
 i := 0;
 j := 0;
  while (i<n) and  (Not singular) Do
    Begin
      i := i+1;
     sumrow^[i] := omvn1v(au^[1+(i-1)*n], n);
      If sumrow^[i]=0
       Then
        singular := true
      Else
        Begin
          h := 2*random-1;
         t^[i] := sumrow^[i]*h;
         h := abs(h);
          If normr<h
           Then
            normr := h
        End
    End;
  k := 0;
  while (k<n) and  not singular Do
    Begin
      k := k+1;
     maxim := 0;
     ipiv := k;
      For i:=k To n Do
        Begin
          h := abs(au^[k+(i-1)*n])/sumrow^[i];
          If maxim<h
           Then
            Begin
              maxim := h;
             ipiv := i
            End
        End;
      If maxim=0
       Then
        singular := true
      Else
        Begin
          k1n := (k-1)*n;
          If ipiv <> k
           Then
            Begin
              ip := 1+(ipiv-1)*n;
             ik := 1+k1n;
              move(au^[ip], row^[1], nsr);
             move(au^[ik], au^[ip], nsr);
              move(row^[1], au^[ik], nsr);
              h := t^[ipiv];
             t^[ipiv] := t^[k];
             t^[k] := h;
              h := px^[ipiv];
             px^[ipiv] := px^[k];
             px^[k] := h;
              sumrow^[ipiv] := sumrow^[k]
            End;
          pivot := au^[k+k1n];
          For i:=k+1 To n Do
            Begin
              i1n := (i-1)*n;
             l := au^[k+i1n]/pivot;
              If l <> 0
               Then
                Begin
                  For j:=k+1 To n Do
                    au^[j+i1n] := au^[j+i1n]-l*au^[j+k1n];
                  t^[i] := t^[i]-l*t^[k];
                  px^[i] := px^[i]-l*px^[k]
                End
            End
        End
    End;
  If  Not singular
   Then
    Begin
      normt := 0;
      For i:=n Downto 1 Do
        Begin
          s := 0;
         i1n := (i-1)*n;
          For j:=i+1 To n Do
            s := s+t^[j]*au^[j+i1n];
          t^[i] := (t^[i]-s)/au^[i+i1n];
          s := 0;
          For j:=i+1 To n Do
            s := s+px^[j]*au^[j+i1n];
          px^[i] := (px^[i]-s)/au^[i+i1n];
          h := abs(t^[i]);
          If normt<h
           Then
            normt := h
        End;
      ca := normt/normr
    End;
   If singular
    Then
     term := 2
   Else
     term := 1;
  freemem(au, sqr(n)*sizeof(ArbFloat));
  freemem(t, nsr);
  freemem(row, nsr);
  freemem(sumrow, nsr);
End; {slegen}
end.

