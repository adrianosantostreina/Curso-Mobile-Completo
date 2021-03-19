{*********************************************************************
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Autor: Brovin Y.D.
 * E-mail: y.brovin@gmail.com
 *
 ********************************************************************}

unit MainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FGX.ActionSheet, FGX.ActionSheet.Types, FMX.StdCtrls,
  FMX.Layouts, System.Actions, FMX.ActnList, FMX.StdActns, FMX.MediaLibrary.Actions, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo, FMX.ListBox, FMX.Edit, FMX.EditBox, FMX.NumberBox;

type
  TFormMain = class(TForm)
    fgActionSheet: TfgActionSheet;
    Button1: TButton;
    SwitchUseUIGuildline: TSwitch;
    Label1: TLabel;
    Label2: TLabel;
    Layout1: TLayout;
    LabelError: TLabel;
    LabelLog: TLabel;
    ActionList: TActionList;
    ActionToSpam: TAction;
    TakePhotoFromCameraAction1: TTakePhotoFromCameraAction;
    TakePhotoFromLibraryAction1: TTakePhotoFromLibraryAction;
    EditVirtualKeyboard1: TVirtualKeyboard;
    MemoLog: TMemo;
    Layout2: TLayout;
    Label3: TLabel;
    ComboBoxTheme: TComboBox;
    LayoutThemeID: TLayout;
    Label4: TLabel;
    NumberBoxThemeID: TNumberBox;
    Layout3: TLayout;
    Label5: TLabel;
    EditTitle: TEdit;
    ClearEditButton1: TClearEditButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure fgActionSheetActions0Click(Sender: TObject);
    procedure Action1Execute(Sender: TObject);
    procedure fgActionSheetHide(Sender: TObject);
    procedure fgActionSheetShow(Sender: TObject);
    procedure fgActionSheetItemClick(Sender: TObject; const AAction: TfgActionCollectionItem);
    procedure ComboBoxThemeChange(Sender: TObject);
    procedure NumberBoxThemeIDChangeTracking(Sender: TObject);
    procedure EditTitleChangeTracking(Sender: TObject);
  private
    procedure UpdateLayoutThemeIDVisible;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

procedure TFormMain.Action1Execute(Sender: TObject);
begin
  LabelLog.Text := 'You click on To Inbox action';
end;

procedure TFormMain.Button1Click(Sender: TObject);
begin
  fgActionSheet.UseUIGuidline := SwitchUseUIGuildline.IsChecked;
  fgActionSheet.Show;
end;

procedure TFormMain.ComboBoxThemeChange(Sender: TObject);
begin
  if ComboBoxTheme.ItemIndex <> -1 then
    fgActionSheet.Theme := TfgActionSheetTheme(ComboBoxTheme.ItemIndex);

  UpdateLayoutThemeIDVisible;
end;

procedure TFormMain.EditTitleChangeTracking(Sender: TObject);
begin
  fgActionSheet.Title := EditTitle.Text;
end;

procedure TFormMain.fgActionSheetActions0Click(Sender: TObject);
begin
  LabelLog.Text := Format('You click on "%s"', [(Sender as TfgActionCollectionItem).Caption]);
end;

procedure TFormMain.fgActionSheetHide(Sender: TObject);
begin
  MemoLog.Lines.Add('TfgActionSheet.OnHide');
end;

procedure TFormMain.fgActionSheetItemClick(Sender: TObject; const AAction: TfgActionCollectionItem);
begin
  MemoLog.Lines.Add(Format('TfgActionSheet.OnItemClick:  caption="%s" index="%d"', [AAction.Caption, AAction.Index]));
end;

procedure TFormMain.fgActionSheetShow(Sender: TObject);
begin
  MemoLog.Lines.Add('TfgActionSheet.OnShow');
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  LabelError.Visible := not fgActionSheet.Supported;
  EditTitle.Text := fgActionSheet.Title;
  NumberBoxThemeID.Value := fgActionSheet.ThemeID;
  ComboBoxTheme.ItemIndex := Integer(fgActionSheet.Theme);
  UpdateLayoutThemeIDVisible;
end;

procedure TFormMain.NumberBoxThemeIDChangeTracking(Sender: TObject);
begin
  fgActionSheet.ThemeID := Round(NumberBoxThemeID.Value);
end;

procedure TFormMain.UpdateLayoutThemeIDVisible;
begin
  LayoutThemeID.Visible := (TOSVersion.Platform = TOSVersion.TPlatform.pfAndroid)
    and (fgActionSheet.Theme = TfgActionSheetTheme.Custom);
end;

end.
