unit Funcoes;

interface

uses
  System.SysUtils;

function ValidaCPF(const ACPF: String): Boolean;

implementation


function ValidaCPF(const ACPF: String): Boolean;
Var
  d1, d4, xx, nCount, resto, digito1, digito2: integer;
  Check: String;
Begin
  if (Length(ACPF) <> 11) then
    Exit(False);

  d1 := 0;
  d4 := 0;
  xx := 1;
  for nCount := 1 to Length(ACPF) - 2 do
  begin
    if Pos(Copy(ACPF, nCount, 1), '/-.') = 0 then
    begin
      d1 := d1 + (11 - xx) * StrToInt(Copy(ACPF, nCount, 1));
      d4 := d4 + (12 - xx) * StrToInt(Copy(ACPF, nCount, 1));
      xx := xx + 1;
    end;
  end;
  resto := (d1 mod 11);
  if resto < 2 then
  begin
    digito1 := 0;
  end
  else
  begin
    digito1 := 11 - resto;
  end;
  d4 := d4 + 2 * digito1;
  resto := (d4 mod 11);
  if resto < 2 then
  begin
    digito2 := 0;
  end
  else
  begin
    digito2 := 11 - resto;
  end;
  Check := IntToStr(digito1) + IntToStr(digito2);
  if Check <> Copy(ACPF, succ(Length(ACPF) - 2), 2) then
  begin
    Result := False;
  end
  else
  begin
    Result := True;
  end;
end;

end.
