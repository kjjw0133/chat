unit Unit9;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Buttons,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Stan.Async, FireDAC.DApt, FireDAC.UI.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Phys, FireDAC.Phys.MySQL, FireDAC.Phys.MySQLDef,
  FireDAC.VCLUI.Wait, Data.DB, FireDAC.Comp.Client, FireDAC.Comp.DataSet;

type
  TForm9 = class(TForm)
    Label5: TLabel;
    FDQueryMembers: TFDQuery;
    FDConnection1: TFDConnection;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label3: TLabel;
    ScrollBox1: TScrollBox;
    procedure FormCreate(Sender: TObject);
  private
    procedure CreateUserLabels(const UserNames: string);
  public
  end;

var
  Form9: TForm9;

implementation

uses Unit1, Unit2, Unit3, Unit8, Unit7;

{$R *.dfm}

procedure TForm9.CreateUserLabels(const UserNames: string);
var
  UserArray: TArray<string>;
  UserPanel: TPanel;
  UserLabel: TLabel;
  I, TopOffset: Integer;
begin
  ScrollBox1.DestroyComponents;

  if Trim(UserNames) = '' then
  begin
    UserLabel := TLabel.Create(ScrollBox1);
    UserLabel.Parent := ScrollBox1;
    UserLabel.Left := 10;
    UserLabel.Top := 10;
    UserLabel.Caption := '참여자가 없습니다.';
    UserLabel.Font.Size := 10;
    UserLabel.Font.Color := clGray;
    Exit;
  end;

  UserArray := UserNames.Split([',']);

  TopOffset := 5;

  for I := 0 to High(UserArray) do
  begin
    UserPanel := TPanel.Create(ScrollBox1);
    UserPanel.Parent := ScrollBox1;
    UserPanel.Left := 5;
    UserPanel.Top := TopOffset;
    UserPanel.Width := ScrollBox1.Width - 25;
    UserPanel.Height := 35;
    UserPanel.BevelOuter := bvNone;
    UserPanel.Color := $00F9F9F9;
    UserPanel.ParentBackground := False;

    UserLabel := TLabel.Create(UserPanel);
    UserLabel.Parent := UserPanel;
    UserLabel.Left := 15;
    UserLabel.Top := 8;
    UserLabel.Caption := '• ' + Trim(UserArray[I]);
    UserLabel.Font.Size := 11;
    UserLabel.Font.Color := clBlack;

    // 현재 로그인한 유저 강조
    if Trim(UserArray[I]) = CurrentUser.Name then
    begin
      UserLabel.Caption := Trim(UserArray[I]) + ' (나)';
      UserLabel.Font.Style := [fsBold];
      UserLabel.Font.Color := clBlue;
      UserPanel.Color := $00FFF8DC;
    end;

    TopOffset := TopOffset + 40;
  end;
end;

procedure TForm9.FormCreate(Sender: TObject);
var
  ChatRoomId, num: Integer;
  chatroomname, chatpw, usernames: String;
begin
  FDQueryMembers.Close;
  FDQueryMembers.SQL.Text :=
    'SELECT c.ChatRoomId, c.chatroomname, c.chatpw, ' +
    'COUNT(DISTINCT cu.UserNo) AS user_count, ' +
    'GROUP_CONCAT(DISTINCT u.name ORDER BY u.name SEPARATOR ", ") AS user_names ' +
    'FROM chat c ' +
    'LEFT JOIN chat_user cu ON c.ChatRoomId = cu.ChatRoomId ' +
    'LEFT JOIN user u ON cu.UserNo = u.userno ' +
    'WHERE c.ChatRoomId = :roomid ' +
    'GROUP BY c.ChatRoomId, c.chatroomname, c.chatpw';

  FDQueryMembers.ParamByName('roomid').AsInteger := Form1.ChatRoomId;
  FDQueryMembers.Open;

  if not FDQueryMembers.IsEmpty then
  begin
    Label1.Caption := '방 번호: ' + FDQueryMembers.FieldByName('ChatRoomId').AsString;
    Label5.Caption := '방 이름: ' + FDQueryMembers.FieldByName('chatroomname').AsWideString;
    Label4.Caption := '인원 수: ' + FDQueryMembers.FieldByName('user_count').AsString + '명';

    if FDQueryMembers.FieldByName('chatpw').AsString <> '' then
      Label2.Caption := '비밀번호: ' + FDQueryMembers.FieldByName('chatpw').AsString
    else
      Label2.Caption := '비밀번호: 없음 (공개방)';

    usernames := FDQueryMembers.FieldByName('user_names').AsString;
    CreateUserLabels(usernames);
  end
  else
  begin
    ShowMessage('채팅방 정보를 불러올 수 없습니다.');
    Close;
  end;
end;

end.
