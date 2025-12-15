unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, FireDAC.Stan.Async, FireDAC.DApt,
  FireDAC.Phys.MySQLDef, FireDAC.UI.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool,
  FireDAC.Phys, FireDAC.Phys.MySQL, FireDAC.VCLUI.Wait, Data.DB,
  FireDAC.Comp.Client, FireDAC.Comp.DataSet, Vcl.ComCtrls, Vcl.ExtCtrls,
  Vcl.Buttons, Vcl.Menus, System.Win.ScktComp;

type
  TForm1 = class(TForm)
    Button1: TButton;
    FDQueryMembers: TFDQuery;
    FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink;
    RichEdit1: TRichEdit;
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    Label1: TLabel;
    Label2: TLabel;
    SpeedButton3: TSpeedButton;
    PopupMenu1: TPopupMenu;
    N4: TMenuItem;
    N2: TMenuItem;
    N1: TMenuItem;
    FDConnection1: TFDConnection;
    ClientSocket1: TClientSocket;
    FDQuery1: TFDQuery;
    Edit1: TEdit;
    Label3: TLabel;
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button1Click(Sender: TObject);
    procedure Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure N4Click(Sender: TObject);
    procedure N2Click(Sender: TObject);
    procedure N1Click(Sender: TObject);
    procedure RichEdit1Change(Sender: TObject);
    procedure ClientSocket1Connect(Sender: TObject; Socket: TCustomWinSocket);
    procedure ClientSocket1Error(Sender: TObject; Socket: TCustomWinSocket;
      ErrorEvent: TErrorEvent; var ErrorCode: Integer);
    procedure ClientSocket1Read(Sender: TObject; Socket: TCustomWinSocket);
  private
    CurrentRoomID: Integer;
    CurrentRoomName: string;
    CurrentRoomNum: Integer;
    Str: string;
    NowStr: string;
    LastDisplayedDate: string;
    procedure LoadChatMessages;
    procedure LoadNewMessages;
    procedure ScrollRichEditToBottom;
    procedure AddChatBubble(const SenderName, Message, Time: string; IsMe: Boolean);
    procedure CheckAndInsertDateSeparator;
  public
    ChatRoomId: Integer;
    UserNo: Integer;
    UserName: String;
    RoomName: String;
    RoomNum: Integer;
    LastChatNo: Integer;
    procedure InitializeChat(RoomId, Num, UserNo: Integer; UserName, RoomName: String);
    procedure JoinChatRoom(ARoomID, ARoomNum: Integer; ARoomName: string);
  end;

var
  Form1: TForm1;

implementation

uses Unit2, Unit3, Unit8, Unit7, Unit9, Unit11;

{$R *.dfm}
const
  WM_NCLBUTTONDOWN = $00A1;
  HTCAPTION = 2;

procedure TForm1.FormCreate(Sender: TObject);
begin


  BorderStyle := bsSingle;
  RichEdit1.Clear;
  Edit1.Clear;
  LastDisplayedDate := '';
end;

procedure TForm1.Panel1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    ReleaseCapture;
    SendMessage(Handle, WM_NCLBUTTONDOWN, HTCAPTION, 0);
  end;
end;

procedure TForm1.AddChatBubble(const SenderName, Message, Time: string; IsMe: Boolean);
begin
  RichEdit1.SelStart := RichEdit1.GetTextLen;

  if IsMe then
  begin
    RichEdit1.Paragraph.Alignment := taRightJustify;

    // 말풍선 전체 여백 (이름 포함)
    RichEdit1.Paragraph.LeftIndent  := 150;
    RichEdit1.Paragraph.RightIndent := 10;
    RichEdit1.Paragraph.FirstIndent := 0;

    // ---- 이름 ----
    RichEdit1.SelAttributes.Size := 8;
    RichEdit1.SelAttributes.Color := clGray;
    RichEdit1.SelAttributes.BackColor := clWhite;
    RichEdit1.SelText := SenderName + #13#10;

    // ---- 말풍선 메시지 ----
    RichEdit1.SelAttributes.Size := 11;
    RichEdit1.SelAttributes.Color := clBlack;
    RichEdit1.SelAttributes.BackColor := $0080FFFF; // 노란색
    RichEdit1.SelText := Message + #13#10;

    // ---- 시간 ----
    RichEdit1.SelAttributes.Size := 7;
    RichEdit1.SelAttributes.Color := clGray;
    RichEdit1.SelAttributes.BackColor := clWhite;
    RichEdit1.SelText := Time + #13#10#13#10;
  end

  else
  begin
    RichEdit1.Paragraph.Alignment := taLeftJustify;

    // 말풍선 전체 여백 (이름 포함)
    RichEdit1.Paragraph.LeftIndent  := 10;
    RichEdit1.Paragraph.RightIndent := 150;
    RichEdit1.Paragraph.FirstIndent := 0;

    // ---- 이름 ----
    RichEdit1.SelAttributes.Size := 8;
    RichEdit1.SelAttributes.Color := clGray;
    RichEdit1.SelAttributes.BackColor := clWhite;
    RichEdit1.SelText := SenderName + #13#10;

    // ---- 말풍선 메시지 ----
    RichEdit1.SelAttributes.Size := 11;
    RichEdit1.SelAttributes.Color := clBlack;
    RichEdit1.SelAttributes.BackColor := $00EFEFEF; // 밝은 회색
    RichEdit1.SelText := Message + #13#10;

    // ---- 시간 ----
    RichEdit1.SelAttributes.Size := 7;
    RichEdit1.SelAttributes.Color := clGray;
    RichEdit1.SelAttributes.BackColor := clWhite;
    RichEdit1.SelText := Time + #13#10#13#10;
  end;

  ScrollRichEditToBottom;
end;

procedure TForm1.CheckAndInsertDateSeparator;
var
  TodayStr, day: string;
  todayCount: Integer;
begin
  TodayStr := FormatDateTime('yyyy-mm-dd', Now);

  // 오늘 날짜 레코드가 이미 존재하는지 확인
  FDQueryMembers.Close;
  FDQueryMembers.SQL.Text :=
    'SELECT COUNT(*) AS cnt FROM chating ' +
    'WHERE ChatRoomId = :roomid AND day = :today';
  FDQueryMembers.ParamByName('roomid').AsInteger := ChatRoomId;
  FDQueryMembers.ParamByName('today').AsString := TodayStr;
  FDQueryMembers.Open;

  todayCount := FDQueryMembers.FieldByName('cnt').AsInteger;

  // 오늘 날짜 레코드가 없고, 마지막 표시 날짜와 다르면 구분선 삽입
  if (todayCount = 0)  then                                         //and (LastDisplayedDate <> TodayStr)
  begin
    RichEdit1.Paragraph.Alignment := taCenter;
    RichEdit1.Paragraph.LeftIndent := 0;
    RichEdit1.Paragraph.RightIndent := 0;
    RichEdit1.SelStart := RichEdit1.GetTextLen;
    RichEdit1.SelAttributes.Size := 9;
    RichEdit1.SelAttributes.Color := clGray;
    RichEdit1.SelAttributes.Style := [fsBold];
    RichEdit1.SelAttributes.BackColor := clWhite;
    day := TodayStr;
    RichEdit1.SelText := #13#10'─────── ' + day + ' ───────'#13#10#13#10;

    LastDisplayedDate := TodayStr;

    // DB에 날짜 레코드 삽입
    try
      FDQueryMembers.Close;
      FDQueryMembers.SQL.Text :=
        'INSERT INTO chating (ChatRoomId, day) VALUES (:roomid, :day)';
      FDQueryMembers.ParamByName('roomid').AsInteger := ChatRoomId;
      FDQueryMembers.ParamByName('day').AsWideString := day;
      FDQueryMembers.ExecSQL;
    except
      // 이미 존재하면 무시 (동시성 문제 대비)
    end;
  end;
end;

procedure TForm1.JoinChatRoom(ARoomID: Integer; ARoomNum: Integer; ARoomName: string);
var
  JoinCmd: string;
  NS: string;
begin
  CurrentRoomID := ARoomID;
  CurrentRoomNum := ARoomNum;
  CurrentRoomName := ARoomName;
  NS := '/chat';

  if ClientSocket1.Active then
  begin
    JoinCmd := Format('JOIN::%d::%s::%s', [CurrentRoomID, CurrentRoomName, NS]);
    ClientSocket1.Socket.SendText(JoinCmd);
  end;
end;

procedure TForm1.InitializeChat(RoomId, Num, UserNo: Integer; UserName, RoomName: String);
var
  JoinMsg: string;
  isNewMember: Boolean;
begin
  Self.ChatRoomId := RoomId;
  Self.RoomNum := Num;
  Self.UserNo := UserNo;
  Self.UserName := UserName;
  Self.RoomName := RoomName;

  LastDisplayedDate := '';

  Label1.Caption := RoomName;
  Label1.Visible := True;
  Label2.Visible := True;

  // 새 멤버인지 확인
  FDQueryMembers.Close;
  FDQueryMembers.SQL.Text :=
    'SELECT COUNT(*) AS cnt FROM chat_user WHERE ChatRoomId = :roomid AND UserNo = :userno';
  FDQueryMembers.ParamByName('roomid').AsInteger := ChatRoomId;
  FDQueryMembers.ParamByName('userno').AsInteger := UserNo;
  FDQueryMembers.Open;

  isNewMember := (FDQueryMembers.FieldByName('cnt').AsInteger = 0);

  // 새 멤버라면 chat_user에만 추가 (num 업데이트 제거!)
  if isNewMember then
  begin
    FDQueryMembers.Close;
    FDQueryMembers.SQL.Text :=
      'INSERT INTO chat_user (ChatRoomId, UserNo) VALUES (:roomid, :userno)';
    FDQueryMembers.ParamByName('roomid').AsInteger := ChatRoomId;
    FDQueryMembers.ParamByName('userno').AsInteger := UserNo;
    FDQueryMembers.ExecSQL;

    // ❌ 기존 코드 (삭제)
    // FDQueryMembers.Close;
    // FDQueryMembers.SQL.Text :=
    //   'UPDATE chat SET num = num + 1 WHERE ChatRoomId = :roomid';
    // FDQueryMembers.ParamByName('roomid').AsInteger := ChatRoomId;
    // FDQueryMembers.ExecSQL;
  end;

  // ✅ 실시간으로 인원수 조회
  FDQueryMembers.Close;
  FDQueryMembers.SQL.Text :=
    'SELECT COUNT(userno) AS num FROM chat_user WHERE ChatRoomId = :roomid';
  FDQueryMembers.ParamByName('roomid').AsInteger := ChatRoomId;
  FDQueryMembers.Open;

  if not FDQueryMembers.IsEmpty then
    Label2.Caption := FDQueryMembers.FieldByName('num').AsString;

  // 이하 동일...
  FDQueryMembers.Close;
  FDQueryMembers.SQL.Text := 'SELECT MAX(c_no) AS lastid FROM chating WHERE ChatRoomId = :roomid';
  FDQueryMembers.ParamByName('roomid').AsInteger := ChatRoomId;
  FDQueryMembers.Open;

  if not FDQueryMembers.IsEmpty then
    LastChatNo := FDQueryMembers.FieldByName('lastid').AsInteger
  else
    LastChatNo := 0;

  LoadChatMessages;

  if isNewMember and ClientSocket1.Active then
  begin
    JoinMsg := Format('JOIN_MSG::%d::%s', [ChatRoomId, UserName]);
    ClientSocket1.Socket.SendText(JoinMsg);
  end;
end;

procedure TForm1.Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) and not (ssShift in Shift) then
  begin
    Button1.Click;
    Key := 0;
  end;
end;

procedure TForm1.N1Click(Sender: TObject);
begin
  Application.CreateForm(TForm9, Form9);
  Form9.ShowModal;
end;

procedure TForm1.N2Click(Sender: TObject);
var
  username : String;
  LeaveMsg: string;
begin
  // 사용자 이름 조회
  FDQueryMembers.Close;
  FDQueryMembers.SQL.Text := 'SELECT name FROM user WHERE userno = :userno';
  FDQueryMembers.ParamByName('userno').AsInteger := CurrentUser.UserNo;
  FDQueryMembers.Open;
  username := FDQueryMembers.FieldByName('name').AsString;

  // 퇴장 메시지를 서버로 전송
  if ClientSocket1.Active then
  begin
    LeaveMsg := Format('LEAVE::%d::%s', [CurrentRoomID, username]);
    ClientSocket1.Socket.SendText(LeaveMsg);
  end;

  // chat_user에서만 삭제 (num 업데이트 제거!)
  FDQueryMembers.Close;
  FDQueryMembers.SQL.Text :=
    'DELETE FROM chat_user WHERE ChatRoomId = :roomid AND UserNo = :userno';
  FDQueryMembers.ParamByName('roomid').AsInteger := ChatRoomId;
  FDQueryMembers.ParamByName('userno').AsInteger := UserNo;
  FDQueryMembers.ExecSQL;

  // ❌ 기존 코드 (삭제)
  // FDQueryMembers.Close;
  // FDQueryMembers.SQL.Text :=
  //   'UPDATE chat SET num = GREATEST(num - 1, 0) WHERE ChatRoomId = :roomid';
  // FDQueryMembers.ParamByName('roomid').AsInteger := ChatRoomId;
  // FDQueryMembers.ExecSQL;

  Label2.Visible := False;
  Label1.Visible := False;

  RichEdit1.Clear;
  Edit1.Clear;

  if ClientSocket1.Active then
    ClientSocket1.Active := False;

  Form7.Show;
  Form1.Close;
end;

procedure TForm1.N4Click(Sender: TObject);
begin
  Application.CreateForm(TForm11, Form11);
  Form11.Show;
end;

procedure TForm1.RichEdit1Change(Sender: TObject);
begin
  ScrollRichEditToBottom;
end;

procedure TForm1.ScrollRichEditToBottom;
const
  WM_VSCROLL = $0115;
  SB_BOTTOM = 7;
begin
  RichEdit1.SelStart := RichEdit1.GetTextLen;
  RichEdit1.SelLength := 0;
  RichEdit1.Perform(WM_VSCROLL, SB_BOTTOM, 0);
  Application.ProcessMessages;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  contents: String;
  userid, nameFromDB: string;
  SendStr: string;
begin
  if not IsLoggedIn then
  begin
    ShowMessage('로그인이 필요합니다.');
    Exit;
  end;

  contents := Trim(Edit1.Text);
  if contents = '' then Exit;

  try
    CheckAndInsertDateSeparator;

    userid := CurrentUser.ID;
    FDQuery1.Close;
    FDQuery1.SQL.Text := 'SELECT name FROM user WHERE id = :userid';
    FDQuery1.ParamByName('userid').AsString := userid;
    FDQuery1.Open;
    nameFromDB := FDQuery1.FieldByName('name').AsString;

    NowStr := FormatDateTime('t', Now);
    Str := Edit1.Text;

    AddChatBubble(nameFromDB, Str, NowStr, True);

    FDQueryMembers.Close;
    FDQueryMembers.SQL.Text :=
      'INSERT INTO chating (ChatRoomId, userno, contents) ' +
      'VALUES (:roomid, :userno, :contents)';
    FDQueryMembers.ParamByName('roomid').AsInteger := ChatRoomId;
    FDQueryMembers.ParamByName('userno').AsInteger := CurrentUser.UserNo;
    FDQueryMembers.ParamByName('contents').AsWideString := contents;
    FDQueryMembers.ExecSQL;

    if ClientSocket1.Active then
    begin
      SendStr := Format('MSG::%d::%s::%s', [CurrentRoomID, nameFromDB, Str]);
      ClientSocket1.Socket.SendText(SendStr);
    end;

    Edit1.Clear;

  except
    on E: Exception do
      ShowMessage('DB 저장 오류: ' + E.Message);
  end;
end;

procedure TForm1.LoadChatMessages;
var
  senderType, senderName, contents, timeStr: string;
  lastDate: string;
begin
  RichEdit1.Clear;
  LastDisplayedDate := '';
  try
    FDQueryMembers.Close;
    FDQueryMembers.SQL.Text :=
      'SELECT ct.c_no, ct.nowtime, u.name, ct.contents, ct.day, ' +
      'CASE WHEN u.userno = :userno THEN ''me'' ELSE ''other'' END AS sender_type ' +
      'FROM chating ct ' +
      'LEFT JOIN user u ON u.userno = ct.userno ' +
      'WHERE ct.ChatRoomId = :roomid ' +
      'ORDER BY ct.nowtime, ct.c_no';
    FDQueryMembers.ParamByName('userno').AsInteger := CurrentUser.UserNo;
    FDQueryMembers.ParamByName('roomid').AsInteger := ChatRoomId;
    FDQueryMembers.Open;
    while not FDQueryMembers.Eof do
    begin
      if FDQueryMembers.FieldByName('contents').IsNull or
         (Trim(FDQueryMembers.FieldByName('contents').AsString) = '') then
      begin
        lastDate := FDQueryMembers.FieldByName('day').AsString;
        RichEdit1.Paragraph.Alignment := taCenter;
        RichEdit1.Paragraph.LeftIndent := 0;
        RichEdit1.Paragraph.RightIndent := 0;
        RichEdit1.SelStart := RichEdit1.GetTextLen;
        RichEdit1.SelAttributes.Size := 9;
        RichEdit1.SelAttributes.Color := clGray;
        RichEdit1.SelAttributes.Style := [fsBold];
        RichEdit1.SelAttributes.BackColor := clWhite;
        RichEdit1.SelText := #13#10'─────── ' + lastDate + ' ───────'#13#10#13#10;
        LastDisplayedDate := lastDate;
      end
      else
      begin
        senderType := FDQueryMembers.FieldByName('sender_type').AsString;
        senderName := FDQueryMembers.FieldByName('name').AsString;
        contents := FDQueryMembers.FieldByName('contents').AsString;
        timeStr := FormatDateTime('t', FDQueryMembers.FieldByName('nowtime').AsDateTime);
        AddChatBubble(senderName, contents, timeStr, (senderType = 'me'));
      end;
      LastChatNo := FDQueryMembers.FieldByName('c_no').AsInteger;
      FDQueryMembers.Next;
    end;
    if LastDisplayedDate = '' then
      LastDisplayedDate := FormatDateTime('yyyy-mm-dd', Now);
  except
    on E: Exception do
      ShowMessage('채팅 불러오기 오류: ' + E.Message);
  end;
end;

procedure TForm1.LoadNewMessages;
var
  senderType, senderName, contents, timeStr: string;
begin
  try
    FDQueryMembers.Close;
    FDQueryMembers.SQL.Text :=
      'SELECT ct.c_no, ct.nowtime, u.name, ct.contents, ct.day, ' +
      'CASE WHEN u.userno = :userno THEN ''me'' ELSE ''other'' END AS sender_type ' +
      'FROM chating ct ' +
      'LEFT JOIN user u ON u.userno = ct.userno ' +
      'WHERE ct.ChatRoomId = :roomid AND ct.c_no > :lastid ' +
      'ORDER BY ct.nowtime, ct.c_no';

    FDQueryMembers.ParamByName('userno').AsInteger := CurrentUser.UserNo;
    FDQueryMembers.ParamByName('roomid').AsInteger := ChatRoomId;
    FDQueryMembers.ParamByName('lastid').AsInteger := LastChatNo;
    FDQueryMembers.Open;

    while not FDQueryMembers.Eof do
    begin
      if FDQueryMembers.FieldByName('contents').IsNull or
         (Trim(FDQueryMembers.FieldByName('contents').AsString) = '') then
      begin
        RichEdit1.Paragraph.Alignment := taCenter;
        RichEdit1.Paragraph.LeftIndent := 0;
        RichEdit1.Paragraph.RightIndent := 0;
        RichEdit1.SelStart := RichEdit1.GetTextLen;
        RichEdit1.SelAttributes.Size := 9;
        RichEdit1.SelAttributes.Color := clGray;
        RichEdit1.SelAttributes.Style := [fsBold];
        RichEdit1.SelAttributes.BackColor := clWhite;
        RichEdit1.SelText := #13#10'─────── ' +
          FDQueryMembers.FieldByName('day').AsString +
          ' ───────'#13#10#13#10;
      end
      else
      begin
        senderType := FDQueryMembers.FieldByName('sender_type').AsString;
        senderName := FDQueryMembers.FieldByName('name').AsString;
        contents := FDQueryMembers.FieldByName('contents').AsString;
        timeStr := FormatDateTime('t', FDQueryMembers.FieldByName('nowtime').AsDateTime);

        AddChatBubble(senderName, contents, timeStr, (senderType = 'me'));
      end;

      LastChatNo := FDQueryMembers.FieldByName('c_no').AsInteger;
      FDQueryMembers.Next;
    end;

  except
    on E: Exception do
      ShowMessage('채팅 새로고침 오류: ' + E.Message);
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  LoadNewMessages;

  try
    // ✅ 실시간 COUNT로 변경
    FDQueryMembers.Close;
    FDQueryMembers.SQL.Text :=
      'SELECT COUNT(userno) AS num FROM chat_user WHERE ChatRoomId = :roomid';
    FDQueryMembers.ParamByName('roomid').AsInteger := ChatRoomId;
    FDQueryMembers.Open;

    if not FDQueryMembers.IsEmpty then
      Label2.Caption := FDQueryMembers.FieldByName('num').AsString;
  except
    on E: Exception do
      ShowMessage('인원 수 갱신 오류: ' + E.Message);
  end;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
var
  contents: String;
begin
  contents := Trim(RichEdit1.Text);
  if contents = '' then
  begin
    FDQueryMembers.SQL.Text := 'delete from chat_user where ChatRoomId = :roomid ';
    FDQueryMembers.ParamByName('roomid').AsInteger := ChatRoomId;
    FDQueryMembers.ExecSQL;

    FDQueryMembers.SQL.Text := 'delete from chat where ChatRoomId = :roomid ';
    FDQueryMembers.ParamByName('roomid').AsInteger := ChatRoomId;
    FDQueryMembers.ExecSQL;
  end
  else
  begin
    Form1.Close;
    Form7.Show;
  end;
end;

procedure TForm1.SpeedButton3Click(Sender: TObject);
var
  P: TPoint;
begin
  P := SpeedButton3.ClientToScreen(Point(0, SpeedButton3.Height));
  PopupMenu1.Popup(P.X, P.Y);
end;

procedure TForm1.ClientSocket1Connect(Sender: TObject; Socket: TCustomWinSocket);
begin
  JoinChatRoom(CurrentRoomID, CurrentRoomNum, CurrentRoomName);
end;

procedure TForm1.ClientSocket1Error(Sender: TObject; Socket: TCustomWinSocket;
  ErrorEvent: TErrorEvent; var ErrorCode: Integer);
begin
  ErrorCode := 0;
  ClientSocket1.Active := False;
end;

procedure TForm1.ClientSocket1Read(Sender: TObject; Socket: TCustomWinSocket);
var
  RecvText: string;
  parts: TArray<string>;
  roomIDStr, senderName, contents: string;
  roomID: Integer;
begin
  RecvText := Socket.ReceiveText;

  if RecvText.StartsWith('MSG::') then
  begin
    parts := RecvText.Split(['::']);
    if Length(parts) >= 4 then
    begin
      roomIDStr := parts[1];
      senderName := parts[2];
      contents := String.Join('::', Copy(parts, 3, Length(parts) - 3));
      roomID := StrToIntDef(roomIDStr, -1);

      if roomID = CurrentRoomID then
      begin
        CheckAndInsertDateSeparator;

        NowStr := FormatDateTime('t', Now);
        AddChatBubble(senderName, contents, NowStr, False);
      end;
    end;
  end

  else if RecvText.StartsWith('JOIN_MSG::') then
  begin
    parts := RecvText.Split(['::']);
    if Length(parts) >= 3 then
    begin
      roomIDStr := parts[1];
      senderName := parts[2];
      roomID := StrToIntDef(roomIDStr, -1);

      if roomID = CurrentRoomID then
      begin
        CheckAndInsertDateSeparator;

        RichEdit1.Paragraph.Alignment := taCenter;
        RichEdit1.Paragraph.LeftIndent := 0;
        RichEdit1.Paragraph.RightIndent := 0;
        RichEdit1.SelStart := RichEdit1.GetTextLen;
        RichEdit1.SelAttributes.Size := 9;
        RichEdit1.SelAttributes.Color := clGreen;
        RichEdit1.SelAttributes.Style := [];
        RichEdit1.SelAttributes.BackColor := clWhite;
        RichEdit1.SelText := senderName + '님이 입장했습니다.'#13#10#13#10;
        ScrollRichEditToBottom;
      end;
    end;
  end

  else if RecvText.StartsWith('LEAVE::') then
  begin
    parts := RecvText.Split(['::']);
    if Length(parts) >= 3 then
    begin
      roomIDStr := parts[1];
      senderName := parts[2];
      roomID := StrToIntDef(roomIDStr, -1);

      if roomID = CurrentRoomID then
      begin
        CheckAndInsertDateSeparator;

        RichEdit1.Paragraph.Alignment := taCenter;
        RichEdit1.Paragraph.LeftIndent := 0;
        RichEdit1.Paragraph.RightIndent := 0;
        RichEdit1.SelStart := RichEdit1.GetTextLen;
        RichEdit1.SelAttributes.Size := 9;
        RichEdit1.SelAttributes.Color := clRed;
        RichEdit1.SelAttributes.Style := [];
        RichEdit1.SelAttributes.BackColor := clWhite;
        RichEdit1.SelText := senderName + '님이 퇴장했습니다.'#13#10#13#10;
        ScrollRichEditToBottom;
      end;
    end;
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  if not ClientSocket1.Active then
  begin
    ClientSocket1.Address := '127.0.0.1';
    ClientSocket1.Port := 8080;
    try
      ClientSocket1.Active := True;
      ShowMessage('서버에 연결되었습니다.');
    except
      on E: Exception do
        ShowMessage('서버 연결 실패: ' + E.Message);
    end;
  end;
end;

end.
