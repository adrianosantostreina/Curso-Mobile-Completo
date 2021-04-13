unit xPlat.OpenPDF;

interface

procedure OpenPDF(const APDFFileName: string; AExternalURL: Boolean = False);

implementation

uses
  System.SysUtils,
  IdURI,
  FMX.Forms,
  System.Classes,
  System.IOUtils,
  FMX.WebBrowser,
  FMX.Types,
  FMX.StdCtrls,
  FMX.Dialogs

  {$IFDEF MSWINDOWS}
  , Winapi.ShellAPI, Winapi.Windows
  {$ENDIF MSWINDOWS}

  {$IFDEF MACOS}
  , Posix.Stdlib
  {$ENDIF MACOS}

  {$IF DEFINED(ANDROID)}
    , Androidapi.JNI.GraphicsContentViewText
    , FMX.Helpers.Android
    , Androidapi.Helpers
    , Androidapi.JNI.Net
    , Androidapi.JNI.JavaTypes
  {$ENDIF}

  {$IF DEFINED(IOS)}
    , iOSApi.Foundation
    , Macapi.Helpers
    , FMX.Helpers.iOS
  {$ENDIF}
  ;

{$IF DEFINED(ANDROID)}
procedure OpenPDF(const APDFFileName: string; AExternalURL: Boolean = False);
var
  Intent         : JIntent;
  Filepath       : String;
  SharedFilePath : string;
  tmpFile        : String;
begin
  if not AExternalURL then
  begin
    Filepath       := TPath.Combine(TPath.GetDocumentsPath      , APDFFileName);
    SharedFilePath := TPath.Combine(TPath.GetSharedDocumentsPath, APDFFileName);

    if TFile.Exists(SharedFilePath) then
      TFile.Delete(SharedFilePath);

    TFile.Copy(Filepath, SharedFilePath);
  end;

  Intent := TJIntent.Create;
  Intent.setAction(TJIntent.JavaClass.ACTION_VIEW);

  tmpFile := StringReplace(APDFFileName, ' ', '%20', [rfReplaceAll]);


  if AExternalURL
  then
//    Intent := TJIntent.JavaClass.init(TJIntent.JavaClass.ACTION_VIEW,
//      TJnet_Uri.JavaClass.parse(StringToJString(TIdURI.URLEncode(tmpFile))))
    Intent.setData(StrToJURI(tmpFile))
  else Intent.setDataAndType(StrToJURI('file://' + SharedFilePath), StringToJString('application/pdf'));

  try
    SharedActivity.startActivity(Intent);
  except on E:Exception do
    ShowMessage('N�o � poss�vel abrir o aquivo PDF' + sLineBreak + Format('[%s] %s', [E.ClassName, E.Message]));
  end;
end;
{$ENDIF}

{$IF DEFINED(iOS)}
type
  TCloseParentFormHelper = class
  public
    procedure OnClickClose(Sender: TObject);
  end;

procedure TCloseParentFormHelper.OnClickClose(Sender: TObject);
begin
  TForm(TComponent(Sender).Owner).Close();
end;

procedure OpenPDF(const APDFFileName: string; AExternalURL: Boolean = False);
var
  NSU                      : NSUrl;
  OK                       : Boolean;
  frm                      : TForm;
  WebBrowser               : TWebBrowser;
  btn                      : TButton;
  btnShare                 : TButton;
  toolSuperior             : TToolBar;
  Evnt                     : TCloseParentFormHelper;
  tmpFile                  : String;
begin
  Frm                      := TForm.CreateNew(nil);

  toolSuperior             := TToolBar.Create(frm);
  toolSuperior.Align       := TAlignLayout.Top;
  toolSuperior.StyleLookup := 'toolbarstyle';
  toolSuperior.Parent      := frm;

  {Bot�o Back}
  btn                      := TButton.Create(frm);
  btn.Align                := TAlignLayout.Left;
  btn.Margins.Left         := 8;
  btn.StyleLookup          := 'backtoolbutton';
  btn.Text                 := 'Voltar';
  btn.Parent               := toolSuperior;

  WebBrowser               := TWebBrowser.Create(frm);
  WebBrowser.Parent        := frm;
  WebBrowser.Align         := TAlignLayout.Client;

  evnt                     := TCloseParentFormHelper.Create;
  btn.OnClick              := evnt.OnClickClose;

  if AExternalURL then
  begin
    tmpFile := StringReplace(APDFFileName, ' ', '%20', [rfReplaceAll]);
    WebBrowser.Navigate('http://' + tmpFile);
  end
  else
    WebBrowser.Navigate('file://' + TPath.Combine(TPath.GetDocumentsPath, APDFFileName));

  frm.ShowModal();
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
procedure OpenPDF(const APDFFileName: string; AExternalURL: Boolean = False);
begin
  ShellExecute(0, 'OPEN', PChar(APDFFileName), '', '', SW_SHOWNORMAL);
end;
{$ENDIF}



{$IFDEF MACOS}
procedure OpenPDF(const APDFFileName: string; AExternalURL: Boolean = False);
begin
  _system(PAnsiChar('open '+'"'+AnsiString(APDFFileName)+'"'));
end;
{$ENDIF MACOS}

end.
