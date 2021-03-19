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

unit FGX.Types.StateValue;

interface

{$SCOPEDENUMS ON}

type

{ TfgStateValue }

  ///  <summary>������� ����� - �������, �������� ��� ��������������� ���������, ������� ���� ���� ������ � �����
  ///  ����������. ��������� ��������� �� ��� ���� ���������� �������� ���������� ����� ������� <c>OnEndUpdate</c> �
  ///  ������� <c>OnEndUpdateCallback</c></summary>
  ///  <remarks>��������, ���� ����� ������������ ��� ��������� ��������� ��������. ���� �� �� �����, ����� ��� ��������
  ///  ���������� ������� �������, ������ ��������� ��������� � �����-���� ���������, �� ��������� ���� ������,
  ///  ��� �������� ����� ��������� ���� ��� �� ��������� ���������.</remarks>
  TfgStateValue = class
  public type
    TfgChangeKind = (BeginUpdate, EndUpdate);
    TfgOnChange = procedure (const AKind: TfgChangeKind) of object;
    TfgOnEndUpdateMethod = procedure of object;
    TfgOnEndUpdateCallback = reference to procedure;
  private
    FUpdatingCount: Integer;
    FOnChange: TfgOnChange;
    FOnEndUpdateMethod: TfgOnEndUpdateMethod;
    FOnEndUpdateCallback: TfgOnEndUpdateCallback;
  protected
    procedure DoChange(const AKind: TfgChangeKind); virtual;
    procedure DoEndUpdate; virtual;
  public
    constructor Create(const AOnEndUpdate: TfgOnEndUpdateMethod = nil); overload;
    constructor Create(const AOnEndUpdateCallback: TfgOnEndUpdateCallback = nil); overload;
    destructor Destroy; override;
    /// <summary>��������������� � ������ �������� ����������.</summary>
    /// <remarks>����� �������� ����� ������ ����. � ���� ������ �� ������ ����� ��������� �������� � ����� <c>EndUpdate</c>.</remarks>
    procedure BeginUpdate;
    /// <summary>��������������� �� ��������� �������� ����������. ���� ������� ���������� �������� <c>IsUpdating = False</c>,
    /// �� ����� ������� ������� <c>OnEndUpdate</c> � ������� <c>OnEndUpdateCallback</c>.</summary>
    /// <remarks>��� ����� ����������� ������ ���� ������ ������� ���, ������� ���� ������� �� <c>BeginUpdate</c>.
    /// ��� ����, ���� ������ �� ��������� � ��������� ����������, �� ��� �� �������� � ������������ �������
    /// <c>OnEndUpdate</c> � ������� <c>OnEndUpdateCallback</c>.</remarks>
    procedure EndUpdate;
    /// <summary>� �������� ����������?</summary>
    function IsUpdating: Boolean;
  public
    property OnChange: TfgOnChange read FOnChange write FOnChange;
    property OnEndUpdateCallback: TfgOnEndUpdateCallback read FOnEndUpdateCallback write FOnEndUpdateCallback;
    property OnEndUpdateMethod: TfgOnEndUpdateMethod read FOnEndUpdateMethod write FOnEndUpdateMethod;
  end;

implementation

uses
  FGX.Asserts;

{ TfgStateValue }

procedure TfgStateValue.DoChange(const AKind: TfgChangeKind);
begin
  if Assigned(FOnChange) then
    FOnChange(AKind);
end;

procedure TfgStateValue.DoEndUpdate;
begin
  if Assigned(FOnEndUpdateMethod) then
    FOnEndUpdateMethod;
  if Assigned(FOnEndUpdateCallback) then
    FOnEndUpdateCallback;
end;

constructor TfgStateValue.Create(const AOnEndUpdate: TfgOnEndUpdateMethod);
begin
  inherited Create;
  FOnEndUpdateMethod := AOnEndUpdate;
end;

constructor TfgStateValue.Create(const AOnEndUpdateCallback: TfgOnEndUpdateCallback);
begin
  inherited Create;
  FOnEndUpdateCallback := AOnEndUpdateCallback;
end;

destructor TfgStateValue.Destroy;
begin
  TfgAssert.IsFalse(IsUpdating, '��� �������� ������� ��������� TfgStateValue ����������, ��� �� ��������� � ��������� ����������. ��� ������� ���, ��� ������ �� ������ ������ EndUpdate �����');

  inherited;
end;

procedure TfgStateValue.BeginUpdate;
begin
  TfgAssert.MoreAndEqulThan(FUpdatingCount, 0);

  DoChange(TfgChangeKind.BeginUpdate);
  Inc(FUpdatingCount);
end;

procedure TfgStateValue.EndUpdate;
begin
  TfgAssert.StrickMoreThan(FUpdatingCount, 0);

  if IsUpdating then
  begin
    DoChange(TfgChangeKind.EndUpdate);
    Dec(FUpdatingCount);
    if FUpdatingCount = 0 then
      DoEndUpdate;
  end;
end;

function TfgStateValue.IsUpdating: Boolean;
begin
  TfgAssert.MoreAndEqulThan(FUpdatingCount, 0);

  Result := FUpdatingCount > 0;
end;

end.
