unit Unit7;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Buttons,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Stan.Async, FireDAC.DApt, FireDAC.UI.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Phys, FireDAC.Phys.MySQL,
  FireDAC.Phys.MySQLDef, FireDAC.VCLUI.Wait, Data.DB,
  FireDAC.Comp.Client, FireDAC.Comp.DataSet;

type
  TForm7 = class(TForm)
    PanelHeader: TPanel;
    LabelTitle: TLabel;
    SpeedButtonMenu: TSpeedButton;
    SpeedButtonSearch: TSpeedButton;
    PanelActions: TPanel;
    Edit2: TEdit;
    Edit1: TEdit;
    Button6: TButton;
    Button3: TButton;
    ScrollBox1: TScrollBox;
    PanelBottom: TPanel;
    Button4: TButton;
    Button1: TButton;
    Button2: TButton;
    FDConnection1: TFDConnection;
    FDQueryMembers: TFDQuery;
    Timer1: TTimer;
    FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink;
    LabelClose: TLabel;
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    procedure Button4Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure PanelHeaderMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LabelCloseClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
  private
    procedure ChatPanelClick(Sender: TObject);
  public
    UserNo : Integer;
    procedure LoadChat(Silent: Boolean = False);
  end;

var
  Form7: TForm7;

implementation

uses unit1,unit2,unit3,unit4,unit5,unit6,unit8,unit10,unit11,unit14,unit17;

{$R *.dfm}

const
  WM_NCLBUTTONDOWN = $00A1;
  HTCAPTION = 2;

procedure TForm7.FormCreate(Sender: TObject);
begin
  BorderStyle := bsNone;

  Timer1.Interval := 5000;
  Timer1.Enabled := False;
end;

procedure TForm7.PanelHeaderMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    ReleaseCapture;
    SendMessage(Handle, WM_NCLBUTTONDOWN, HTCAPTION, 0);
  end;
end;

procedure TForm7.SpeedButton1Click(Sender: TObject);
var
  FriendForm: TForm14;
begin
  FDQueryMembers.Close;
  FDQueryMembers.SQL.Text := ' select is_logged_in from user where userno = :userno ';
  FDQueryMembers.ParamByName('userno').AsInteger := CurrentUser.UserNo;
  FDQueryMembers.Open;

  if not FDQueryMembers.FieldByName('is_logged_in').AsBoolean then
  begin
    if not IsLoggedIn then
    begin
      ShowMessage('로그인이 필요합니다.');
      Exit;
    end;
  end;

  Self.Hide;
  Application.ProcessMessages;  // Hide가 완전히 처리되도록 대기

  // Form14 생성 및 표시
  FriendForm := TForm14.Create(Application);
  try
    FriendForm.Position := poScreenCenter;
    FriendForm.ShowModal;
  finally
    FriendForm.Free;
  end;

  Self.Show;
  Self.BringToFront;
end;

// Unit7.pas의 SpeedButton3Click 수정

procedure TForm7.SpeedButton3Click(Sender: TObject);
var
  FriendRequestForm: TForm17;
begin
  // 로그인 체크
  FDQueryMembers.Close;
  FDQueryMembers.SQL.Text := 'select is_logged_in from user where userno = :userno';
  FDQueryMembers.ParamByName('userno').AsInteger := CurrentUser.UserNo;
  FDQueryMembers.Open;

  if not FDQueryMembers.FieldByName('is_logged_in').AsBoolean then
  begin
    if not IsLoggedIn then
    begin
      ShowMessage('로그인이 필요합니다.');
      Exit;
    end;
  end;

  // Form17 생성 및 표시
  FriendRequestForm := TForm17.Create(Application);
  try
    FriendRequestForm.Position := poScreenCenter;
    FriendRequestForm.ShowModal;
  finally
    FriendRequestForm.Free;
  end;
end;

procedure TForm7.LabelCloseClick(Sender: TObject);
begin
  close;
end;

procedure TForm7.LoadChat(Silent: Boolean = False);
var
  ChatPanel: TPanel;
  RoomNameLabel, MemberCountLabel, DateLabel, TimeLabel, LastMsgLabel: TLabel;
  TopOffset: Integer;
  RoomID: Integer;
  FDQuery1: TFDQuery;
begin
  FDQueryMembers.Close;
  FDQueryMembers.SQL.Text :=
    'SELECT chating.c_no, chat_user.ChatRoomId, '+
    'chat.chatroomname AS room_name,  ' +
    'DATE_FORMAT(chating.nowtime, "%Y-%m-%d") AS date, '+
    'DATE_FORMAT(chating.nowtime, "%H:%i") AS time, ' +
    'chating.contents AS last_message FROM chat_user '+
    'JOIN chat ON chat_user.ChatRoomId = chat.ChatRoomId '+
    'JOIN ( SELECT c.* FROM chating c JOIN ( '+
    'SELECT ChatRoomId, MAX(nowtime) AS latest_time '+
    'FROM chating GROUP BY ChatRoomId ' +
    ') latest ON c.ChatRoomId = latest.ChatRoomId  ' +
    'AND c.nowtime = latest.latest_time '+
    ') chating ON chat_user.ChatRoomId = chating.ChatRoomId '+
    'WHERE chat_user.userno = :userno AND chating.contents IS NOT NULL '+
    'ORDER BY chating.nowtime asc ';

  FDQueryMembers.ParamByName('userno').AsInteger := UserNo;
  FDQueryMembers.Open;

  ScrollBox1.DestroyComponents;

  FDQuery1 := TFDQuery.Create(nil);
  try
    FDQuery1.Connection := FDConnection1;

    TopOffset := 10;
    while not FDQueryMembers.Eof do
    begin
      RoomID := FDQueryMembers.FieldByName('ChatRoomId').AsInteger;

      ChatPanel := TPanel.Create(ScrollBox1);
      ChatPanel.Parent := ScrollBox1;
      ChatPanel.Align := alTop;
      ChatPanel.Height := 60;
      ChatPanel.Caption := '';
      ChatPanel.Tag := RoomID;
      ChatPanel.OnClick := ChatPanelClick;

      RoomNameLabel := TLabel.Create(ChatPanel);
      RoomNameLabel.Parent := ChatPanel;
      RoomNameLabel.Left := 10;
      RoomNameLabel.Top := 8;
      RoomNameLabel.Font.Style := [fsBold];
      RoomNameLabel.Caption := FDQueryMembers.FieldByName('room_name').AsString;
      RoomNameLabel.Tag := ChatPanel.Tag;
      RoomNameLabel.OnClick := ChatPanelClick;

      DateLabel := TLabel.Create(ChatPanel);
      DateLabel.Parent := ChatPanel;
      DateLabel.Left := 10;
      DateLabel.Top := 30;
      DateLabel.Caption := FDQueryMembers.FieldByName('date').AsString + ' ' +
                           FDQueryMembers.FieldByName('time').AsString;
      DateLabel.Tag := ChatPanel.Tag;
      DateLabel.OnClick := ChatPanelClick;

      LastMsgLabel := TLabel.Create(ChatPanel);
      LastMsgLabel.Parent := ChatPanel;
      LastMsgLabel.Left := 200;
      LastMsgLabel.Top := 30;
      LastMsgLabel.Caption := FDQueryMembers.FieldByName('last_message').AsString;
      LastMsgLabel.Tag := ChatPanel.Tag;
      LastMsgLabel.OnClick := ChatPanelClick;

      FDQuery1.Close;
      FDQuery1.SQL.Text :=
        'SELECT COUNT(userno) AS num FROM chat_user WHERE ChatRoomId = :Roomid';
      FDQuery1.ParamByName('RoomId').AsInteger := RoomID;
      FDQuery1.Open;

      MemberCountLabel := TLabel.Create(ChatPanel);
      MemberCountLabel.Parent := ChatPanel;
      MemberCountLabel.Left := 200;
      MemberCountLabel.Top := 8;
      MemberCountLabel.Caption := Format('인원: %d명', [FDQuery1.FieldByName('num').AsInteger]);
      MemberCountLabel.Tag := ChatPanel.Tag;
      MemberCountLabel.OnClick := ChatPanelClick;

      TopOffset := TopOffset + ChatPanel.Height + 5;
      FDQueryMembers.Next;
    end;

    if (FDQueryMembers.RecordCount = 0) and (not Silent) then
      ShowMessage('참여 중인 채팅방이 없습니다.');

  finally
    FDQuery1.Free;
  end;

  FDQueryMembers.Close;
end;

procedure TForm7.Timer1Timer(Sender: TObject);
begin
  try
    LoadChat(True);
  except
  end;
end;

procedure TForm7.ChatPanelClick(Sender: TObject);
var
  RoomID: Integer;
  qryUserNo, qryNum: Integer;
  qryChatRoomId: Integer;
  qryChatRoomName, qryName: string;
begin
  FDQueryMembers.Close;
  FDQueryMembers.SQL.Text := 'select is_logged_in from user where userno = :userno ';
  FDQueryMembers.ParamByName('userno').AsInteger := userno;
  FDQueryMembers.Open;

  if not FDQueryMembers.FieldByName('is_logged_in').AsBoolean then
  begin
    if not IsLoggedIn then
    begin
      ShowMessage('로그인이 필요합니다.');
      close;
      Exit;
    end;
  end;

  if Sender is TPanel then
    RoomID := (Sender as TPanel).Tag
  else if Sender is TLabel then
    RoomID := (Sender as TLabel).Tag
  else
    Exit;

  FDQueryMembers.Close;
  FDQueryMembers.SQL.Text :=
    ' SELECT cu.ChatRoomId, u.userno, c.chatroomname, u.name '+
    ' FROM chat_user cu '+
    ' JOIN chat c ON c.ChatRoomId = cu.ChatRoomId '+
    ' JOIN user u ON u.userno = cu.userno '+
    ' WHERE c.ChatRoomId = :RoomId AND u.userno = :UserNo ';

  FDQueryMembers.ParamByName('RoomId').AsInteger := RoomID;
  FDQueryMembers.ParamByName('UserNo').AsInteger := Self.UserNo;
  FDQueryMembers.Open;

  try
    if FDQueryMembers.IsEmpty then
    begin
      ShowMessage('해당 방의 정보가 없습니다.');
      Exit;
    end;

    qryChatRoomId := FDQueryMembers.FieldByName('ChatRoomId').AsInteger;
    qryUserNo := FDQueryMembers.FieldByName('userno').AsInteger;
    qryChatRoomName := FDQueryMembers.FieldByName('chatroomname').AsString;
    qryName := FDQueryMembers.FieldByName('name').AsString;

    FDQueryMembers.Close;
    FDQueryMembers.SQL.Text :=
      'SELECT COUNT(userno) AS num FROM chat_user WHERE ChatRoomId = :Roomid';
    FDQueryMembers.ParamByName('RoomId').AsInteger := RoomID;
    FDQueryMembers.Open;

    qryNum := FDQueryMembers.FieldByName('num').AsInteger;

    if not Form1.ClientSocket1.Active then
    begin
      try
        Form1.ClientSocket1.Address := '127.0.0.1';
        Form1.ClientSocket1.Port := 8080;
        Form1.ClientSocket1.Active := True;
        Sleep(100);
      except
        on E: Exception do
        begin
          ShowMessage('서버 연결 실패: ' + E.Message);
          Exit;
        end;
      end;
    end;

    Form1.JoinChatRoom(qryChatRoomId, qryNum, qryChatRoomName);
    Form1.InitializeChat(qryChatRoomId, qryNum, qryUserNo, qryName, qryChatRoomName);
    ShowMessage('채팅방 ' + IntToStr(qryChatRoomId) + ' 입장 (방명: ' + qryChatRoomName + ')');

    Form1.Position := poDesigned;
    Form1.Show;
    Self.Hide;
  finally
    FDQueryMembers.Close;
  end;
end;

procedure TForm7.Button2Click(Sender: TObject);
begin
  FDQueryMembers.Close;
  FDQueryMembers.SQL.Text := 'select is_logged_in from user where userno = :userno ';
  FDQueryMembers.ParamByName('userno').AsInteger := userno;
  FDQueryMembers.Open;

  if not FDQueryMembers.FieldByName('is_logged_in').AsBoolean then
  begin
    if not IsLoggedIn then
    begin
      ShowMessage('로그인이 필요합니다.');
      Exit;
    end;
  end;

  Application.CreateForm(TForm11, Form11);
  Form11.Show;
end;

procedure TForm7.Button3Click(Sender: TObject);
begin
  FDQueryMembers.Close;
  FDQueryMembers.SQL.Text := 'select is_logged_in from user where userno = :userno ';
  FDQueryMembers.ParamByName('userno').AsInteger := userno;
  FDQueryMembers.Open;

  if not FDQueryMembers.FieldByName('is_logged_in').AsBoolean then
  begin
    if not IsLoggedIn then
    begin
      ShowMessage('로그인이 필요합니다.');
      Exit;
    end;
  end;
  Form8.Show;
end;

procedure TForm7.Button1Click(Sender: TObject);
begin
  if not IsLoggedIn then
  begin
    if Form3.ShowModal = mrOk then
    begin
      IsLoggedIn := True;

      Edit1.Enabled   := True;
      Edit2.Enabled   := True;
      Button6.Enabled := True;
      Button2.Enabled := True;
      Button2.Enabled := True;
      Button3.Enabled := True;
      Button4.Caption := '로그아웃';
      LoadChat;

      Timer1.Interval := 5000;
      Timer1.Enabled := True;
    end;
  end
  else
  begin
    IsLoggedIn := False;
    CurrentUser.ID := '';
    CurrentUser.Name := '';
    CurrentUser.Role := '';

    Edit1.Enabled   := False;
    Edit2.Enabled   := False;
    Button6.Enabled := False;
    Button2.Enabled := True;
    Button3.Enabled := False;
    Button4.Caption := '2';

    ScrollBox1.DestroyComponents;
    ShowMessage('로그아웃 되었습니다.');

    Timer1.Enabled := False;
  end;
end;

procedure TForm7.Button4Click(Sender: TObject);
begin
    FDQueryMembers.Close;
    FDQueryMembers.SQL.Text := 'select is_logged_in from user where userno = :userno';
    FDQueryMembers.ParamByName('userno').AsInteger := CurrentUser.UserNo;
    FDQueryMembers.Open;

  begin
    if not IsLoggedIn then
    begin
      if Form2.ShowModal = mrOk then
      begin
        IsLoggedIn := True;

        Button2.Left := 230;
        Button4.left := 104;
        Edit1.Enabled   := True;
        Edit2.Enabled   := True;
        Button6.Enabled := True;
        Button2.Enabled := True;
        Button3.Enabled := True;
        Button1.Visible := False;
        Button1.Enabled := False;
        Button4.Caption := '로그아웃';
        LoadChat;

        Timer1.Interval := 5000;
        Timer1.Enabled := True;
      end;
    end
    else if FDQueryMembers.FieldByName('is_logged_in').AsBoolean then
    begin
      if IsLoggedIn then
        begin
          Timer1.Enabled := False;

          FDQueryMembers.Close;
          FDQueryMembers.SQL.Text := 'update user set is_logged_in = 0 where userno= :userno';
          FDQueryMembers.ParamByName('userno').AsInteger := CurrentUser.UserNo;
          FDQueryMembers.ExecSQL;

          IsLoggedIn := False;
          CurrentUser.ID := '';
          CurrentUser.Name := '';
          CurrentUser.Role := '';

          Button2.Left := 284;
          Button4.left := 32;
          Edit1.Enabled   := False;
          Edit1.Clear;
          Edit2.Clear;
          Edit2.Enabled   := False;
          Button3.Enabled := True;
          Button6.Enabled := True;
          Button1.Visible := True;
          Button1.Enabled := True;
          Button4.Caption := '로그인';

          ScrollBox1.DestroyComponents;
          ShowMessage('로그아웃 되었습니다.');

          Timer1.Enabled := False;
        end;
    end

  end;

end;

procedure TForm7.Button6Click(Sender: TObject);
var
  ChatRoomID, num, userno: Integer;
  chatpw, chatroomname: String;
begin

  FDQueryMembers.Close;
  FDQueryMembers.SQL.Text := 'select is_logged_in from user where userno = :userno ';
  FDQueryMembers.ParamByName('userno').AsInteger := userno;
  FDQueryMembers.Open;

  if not FDQueryMembers.FieldByName('is_logged_in').AsBoolean then
  begin
    if not IsLoggedIn then
    begin
      ShowMessage('로그인이 필요합니다.');
      Exit;
    end;
  end;

  userno := CurrentUser.UserNo;

  if not TryStrToInt(Trim(Edit2.Text), ChatRoomID) then
  begin
    ShowMessage('유효한 방 번호를 입력하세요.');
    Exit;
  end;

  chatpw := Trim(Edit1.Text);

  FDQueryMembers.Close;
  FDQueryMembers.SQL.Text :=
    'SELECT c.chatpw, COUNT(cu.userno) AS num, c.ChatRoomId, c.chatroomname ' +
    'FROM chat c ' +
    'LEFT JOIN chat_user cu ON c.ChatRoomId = cu.ChatRoomId ' +
    'WHERE c.chatpw = :chatpw AND c.ChatRoomId = :ChatRoomID ' +
    'GROUP BY c.ChatRoomId';
  FDQueryMembers.ParamByName('chatpw').AsString := chatpw;
  FDQueryMembers.ParamByName('ChatRoomID').AsInteger := ChatRoomID;
  FDQueryMembers.Open;

  if FDQueryMembers.IsEmpty then
  begin
    ShowMessage('비밀번호가 일치하지 않거나 존재하지 않는 방입니다.');
    Exit;
  end;

  chatroomname := FDQueryMembers.FieldByName('chatroomname').AsString;
  num := FDQueryMembers.FieldByName('num').AsInteger;

  if not Form1.ClientSocket1.Active then
  begin
    try
      Form1.ClientSocket1.Address := '127.0.0.1';
      Form1.ClientSocket1.Port := 8080;
      Form1.ClientSocket1.Active := True;
      Sleep(100);
    except
      on E: Exception do
      begin
        ShowMessage('서버 연결 실패: ' + E.Message);
        Exit;
      end;
    end;
  end;

  Form1.JoinChatRoom(ChatRoomID, num, chatroomname);
  Form1.InitializeChat(ChatRoomID, num, userno, CurrentUser.Name, chatroomname);
  Form1.Show;
  Self.Hide;
end;

end.
