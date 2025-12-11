unit Unit14;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.ComCtrls, math, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Stan.Async, FireDAC.DApt, FireDAC.UI.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Phys, FireDAC.Phys.MySQL,
  FireDAC.Phys.MySQLDef, FireDAC.VCLUI.Wait, Data.DB, FireDAC.Comp.Client,
  FireDAC.Comp.DataSet, System.Generics.Collections, Vcl.Buttons,
  FriendManager;

type
  TForm14 = class(TForm)
    PanelBottom: TPanel;
    userCountLabel: TLabel;
    PanelHeader: TPanel;
    LabelTitle: TLabel;
    SpeedButtonSearch: TSpeedButton;
    LabelClose: TLabel;
    PanelActions: TPanel;
    Edit2: TEdit;
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    FDConnection1: TFDConnection;
    FDQueryMembers: TFDQuery;
    Timer1: TTimer;
    FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink;
    lbFriends: TListBox;
    Button1: TButton;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure lbFriendsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure lbFriendsMeasureItem(Control: TWinControl; Index: Integer;
      var Height: Integer);
    procedure SpeedButton2Click(Sender: TObject);
    procedure LabelCloseClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FFriendManager: TFriendManager;
    FFilteredFriends: TList<TFriendInfo>;
    procedure LoadFriendsList;
    procedure FilterFriendsList(const Keyword: string);
  public
  end;

implementation

uses unit2, unit16;

{$R *.dfm}

const
  AVATAR_SIZE = 36;
  ITEM_PADDING = 8;

procedure TForm14.FormCreate(Sender: TObject);
begin
  FFriendManager := TFriendManager.Create(FDConnection1);
  FFilteredFriends := TList<TFriendInfo>.Create;

  lbFriends.Style := lbOwnerDrawVariable;
  lbFriends.DoubleBuffered := True;

  LoadFriendsList;
end;

procedure TForm14.FormDestroy(Sender: TObject);
begin
  FFilteredFriends.Free;
  FFriendManager.Free;
end;

procedure TForm14.LoadFriendsList;
begin
  if FFriendManager.LoadFriends(CurrentUser.UserNo) then
  begin
    FilterFriendsList('');
  end
  else
  begin
    ShowMessage('친구 목록을 불러올 수 없습니다.');
  end;
end;

procedure TForm14.SpeedButton2Click(Sender: TObject);
begin
  Close;
end;

procedure TForm14.LabelCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TForm14.FilterFriendsList(const Keyword: string);
var
  Friend: TFriendInfo;
begin
  lbFriends.Items.BeginUpdate;
  try
    lbFriends.Items.Clear;

    if Assigned(FFilteredFriends) then
      FFilteredFriends.Free;

    FFilteredFriends := FFriendManager.FilterByKeyword(Keyword);

    for Friend in FFilteredFriends do
    begin
      lbFriends.Items.Add(Friend.UserName);
    end;

    userCountLabel.Caption := '친구 ' + IntToStr(FFilteredFriends.Count) + '명';
  finally
    lbFriends.Items.EndUpdate;
  end;
end;

procedure TForm14.Button1Click(Sender: TObject);
begin
   if not Assigned(Form16) then
    Form16 := TForm16.Create(Application);
     try
       Form16.ShowModal;
     finally
       FreeAndNil(Form16);
     end;

end;

procedure TForm14.Edit2Change(Sender: TObject);
begin
  FilterFriendsList(Trim(Edit2.Text));
end;

procedure TForm14.lbFriendsMeasureItem(Control: TWinControl; Index: Integer;
  var Height: Integer);
begin
  Height := Max(AVATAR_SIZE + ITEM_PADDING, 46);
end;

procedure TForm14.lbFriendsDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  C: TCanvas;
  sName: String;
  YCenter: Integer;
  avatarRect: TRect;
  colBg: TColor;
  TextLeft: Integer;
  Friend: TFriendInfo;
begin
  if (Index < 0) or (Index >= FFilteredFriends.Count) then
    Exit;

  C := lbFriends.Canvas;
  Friend := FFilteredFriends[Index];
  sName := Friend.UserName;

  if odSelected in State then
    C.Brush.Color := $00F0F8FF
  else
    C.Brush.Color := clWhite;
  C.FillRect(Rect);

  YCenter := (Rect.Top + Rect.Bottom) div 2;

  avatarRect.Left := Rect.Left + ITEM_PADDING;
  avatarRect.Right := avatarRect.Left + AVATAR_SIZE;
  avatarRect.Top := YCenter - AVATAR_SIZE div 2;
  avatarRect.Bottom := avatarRect.Top + AVATAR_SIZE;

  if Length(sName) > 0 then
  begin
    colBg := RGB(
      150 + (Ord(sName[1]) * 3) mod 100,
      120 + (Ord(sName[1]) * 5) mod 100,
      160 + (Ord(sName[1]) * 7) mod 80
    );
  end
  else
    colBg := RGB(150, 120, 160);

  C.Brush.Color := colBg;
  C.Pen.Color := colBg;
  C.Ellipse(avatarRect.Left, avatarRect.Top, avatarRect.Right, avatarRect.Bottom);

  C.Font.Name := 'Malgun Gothic';
  C.Font.Size := 10;
  C.Font.Style := [fsBold];
  C.Font.Color := clWhite;

  if Length(sName) > 0 then
    TextOut(C.Handle,
            avatarRect.Left + (AVATAR_SIZE - C.TextWidth(Copy(sName,1,1))) div 2,
            avatarRect.Top + (AVATAR_SIZE - C.TextHeight(Copy(sName,1,1))) div 2,
            PChar(Copy(sName,1,1)), 1);

  TextLeft := avatarRect.Right + ITEM_PADDING;

  C.Font.Name := 'Malgun Gothic';
  C.Font.Size := 11;
  C.Font.Style := [];
  C.Font.Color := clWindowText;
  C.Brush.Style := bsClear;

  TextOut(C.Handle,
          TextLeft,
          YCenter - C.TextHeight(sName) div 2,
          PChar(sName), Length(sName));
end;

end.
