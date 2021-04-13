unit untLib;

interface

uses
  System.RegularExpressions,
  System.SysUtils,
  System.UITypes,
  System.IOUtils,

  FMX.Edit,
  FMX.Objects,

  ksTabControl,

  IniFiles,


  Constantes;

type
  TLibrary = class
  private
    class var FOnline: Boolean;
    class var FUsuario: String;
    class var FLembrar: Boolean;
    { private declarations }
  protected
    { protected declarations }
  public

    {Métodos}
    class procedure MudarAba(const AksTabCtrl: TksTabControl; const AksTabItem: TksTabItem);
    class procedure ValidarCampo(const ACampo: TEdit; const ALinha: TLine; const ATipoValidacao: TTipoValidacao);
    class procedure LerConfig;
    class procedure SalvarConfig(const AUsuario : String; ALembrar : Boolean);


    {Propriedades}
    class property Online : Boolean read FOnline write FOnline;
    class property Usuario : String read FUsuario write FUsuario;
    class property Lembrar : Boolean read FLembrar write FLembrar;
  
  published
    { published declarations }
  end;

implementation



{ TLibrary }

class procedure TLibrary.LerConfig;
var
  IniFile  : TiniFile;
  SUsuario : String;
  bLembrar : Boolean;
begin
  try
    try
      IniFile  := TIniFile.Create(System.IOUtils.TPath.Combine(System.IOUtils.TPath.GetDocumentsPath, 'Config.ini'));    
      FUsuario := IniFile.ReadString('Credenciais','Usuario',FUsuario);
      FLembrar := IniFile.ReadBool('Credenciais','Lembrar',FLembrar);
      
    except on E: Exception do
      begin
      
      end;
    end;
  finally
  end;

end;

class procedure TLibrary.MudarAba(const AksTabCtrl: TksTabControl; const AksTabItem: TksTabItem);
begin
  AksTabCtrl.FadeToTab(AksTabItem,0.3);
end;

class procedure TLibrary.SalvarConfig(const AUsuario : String; ALembrar : Boolean);
var
  IniFile  : TiniFile;
begin
  try
    try
      IniFile  := TIniFile.Create(System.IOUtils.TPath.Combine(System.IOUtils.TPath.GetDocumentsPath, 'Config.ini'));    
      IniFile.WriteString('Credenciais','Usuario',AUsuario);
      IniFile.WriteBool('Credenciais','Lembrar',ALembrar);
    except on E: Exception do
    end;
  finally
    Inifile.disposeof;
  end;
end;

class procedure TLibrary.ValidarCampo(const ACampo: TEdit; const ALinha: TLine; const ATipoValidacao: TTipoValidacao);
begin
  case ATipoValidacao of
    tvCPF:
      begin
        if TEdit(ACampo).Text.Equals(EmptyStr) then
        begin
          TLine(ALinha).Stroke.Color := TAlphaColors.White;
        end
        else
        begin
          if TRegEx.IsMatch(TEdit(ACampo).Text, C_EXP_CPF) then
          begin
            TLine(ALinha).Stroke.Color := TAlphaColors.Springgreen;
          end
          else
          begin
            TLine(ALinha).Stroke.Color := TAlphaColors.Red;    
          end;
        end;
      
      end;
    tvEmail:
      begin
        if TEdit(ACampo).Text.Equals(EmptyStr) then
        begin
          TLine(ALinha).Stroke.Color := TAlphaColors.White;
        end
        else
        begin
          if TRegEx.IsMatch(TEdit(ACampo).Text, C_EXP_EMAIL) then
          begin
            TLine(ALinha).Stroke.Color := TAlphaColors.Springgreen;
          end
          else
          begin
            TLine(ALinha).Stroke.Color := TAlphaColors.Red;    
          end;
        end;
      end;
  end;
end;

end.
