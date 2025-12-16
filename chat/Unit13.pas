unit Unit13;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.ComCtrls, math, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Stan.Async, FireDAC.DApt, FireDAC.UI.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Phys, FireDAC.Phys.MySQL,
  FireDAC.Phys.MySQLDef, FireDAC.VCLUI.Wait, Data.DB, FireDAC.Comp.Client,
  FireDAC.Comp.DataSet, System.Generics.Collections;

type
  TFriendInfo = class
    UserId: String;
    UserNo: Integer;
    UserName: WideString;
    IsSelected: Boolean;  // 다중 선택을 위한 필드 추가
  end;

  TForm13 = class(TForm)
    LabelTitle: TLabel;
    pnlSearch: TPanel;
    pbSearchBG: TPaintBox;
    edtSearch: TEdit;
    lbFriends: TListBox;
    PanelBottom: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    FDQuery1: TFDQuery;
    FDConnection1: TFDConnection;
    FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink;
    userCountLabel: TLabel;

    procedure FormCreate(Sender: TObject);
    procedure pbSearchBGPaint(Sender: TObject);
    procedure edtSearchChange(Sender: TObject);
    procedure edtSearchEnter(Sender: TObject);
    procedure edtSearchExit(Sender: TObject);
    procedure lbFriendsMeasureItem(Control: TWinControl; Index: Integer;
      var Height: Integer);
    procedure lbFriendsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure lbFriendsClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    FAllFriends: TObjectList<TFriendInfo>;
    FFilteredFriends: TObjectList<TFriendInfo>;
    procedure PopulateAllFriends;
    procedure FilterList(const Keyword: string);
    procedure DrawMagnifier(const C: TCanvas; const R: TRect);
    function GetSelectedFriend: TFriendInfo;
    function GetSelectedFriends: TList<TFriendInfo>;  // 다중 선택용
  public
  end;

var
  Form13: TForm13;
  status: Boolean = False;

implementation

{$R *.dfm}
uses unit8, Unit7, unit2, unit1;

const
  AVATAR_SIZE = 36;
  ITEM_PADDING = 8;
  RADIUS = 24;

procedure TForm13.FormCreate(Sender: TObject);
var
  Rgn: HRGN;
begin
  FAllFriends := TObjectList<TFriendInfo>.Create(True);
  FFilteredFriends := TObjectList<TFriendInfo>.Create(False);

  // MySQL 연결 시 UTF-8 문자셋 설정
  if not FDConnection1.Connected then
  begin
    FDConnection1.Params.Values['CharacterSet'] := 'utf8mb4';
    FDConnection1.Connected := True;
  end;

  PopulateAllFriends;

  lbFriends.Style := lbOwnerDrawVariable;
  lbFriends.DoubleBuffered := True;

  FilterList('');

  pbSearchBG.SendToBack;

  Rgn := CreateRoundRectRgn(0, 0, pnlSearch.Width + 1, pnlSearch.Height + 1,
                            pnlSearch.Height, pnlSearch.Height);
  SetWindowRgn(pnlSearch.Handle, Rgn, True);

  userCountLabel.Caption := '친구 ' + IntToStr(FAllFriends.Count) + '명';
end;

procedure TForm13.FormDestroy(Sender: TObject);
begin
  FFilteredFriends.Free;
  FAllFriends.Free;
end;

{ ---------------- 데이터베이스에서 친구 목록 불러오기 ---------------- }
procedure TForm13.PopulateAllFriends;
var
  FriendInfo: TFriendInfo;
  CurrentUserId: String;
  NameStr: String;
begin
  FAllFriends.Clear;

  // CurrentUser.UserNo로 user.id를 조회해야 함
  try
    // UTF-8 설정 확인
    if not FDConnection1.Connected then
    begin
      FDConnection1.Params.Values['CharacterSet'] := 'utf8mb4';
      FDConnection1.Connected := True;
    end;

    FDQuery1.Close;
    FDQuery1.SQL.Text := 'SELECT id FROM user WHERE userno = :userno';
    FDQuery1.ParamByName('userno').AsInteger := CurrentUser.UserNo;
    FDQuery1.Open;

    if FDQuery1.RecordCount = 0 then
    begin
      ShowMessage('현재 사용자 정보를 찾을 수 없습니다.');
      Exit;
    end;

    CurrentUserId := FDQuery1.FieldByName('id').AsString;
    FDQuery1.Close;

    // friend 테이블은 user.id를 참조하므로 id로 조회
    FDQuery1.SQL.Text :=
      'SELECT ' +
      '  CASE ' +
      '    WHEN f.requester_id = :current_user_id THEN u2.userno ' +
      '    ELSE u1.userno ' +
      '  END AS friend_userno, ' +
      '  CASE ' +
      '    WHEN f.requester_id = :current_user_id THEN f.receiver_id ' +
      '    ELSE f.requester_id ' +
      '  END AS friend_id, ' +
      '  CASE ' +
      '    WHEN f.requester_id = :current_user_id THEN u2.name ' +
      '    ELSE u1.name ' +
      '  END AS friend_name ' +
      'FROM friend f ' +
      'LEFT JOIN user u1 ON f.requester_id = u1.id ' +
      'LEFT JOIN user u2 ON f.receiver_id = u2.id ' +
      'WHERE (f.requester_id = :current_user_id OR f.receiver_id = :current_user_id) ' +
      '  AND f.status = 2 ' +
      'ORDER BY friend_name';

    FDQuery1.ParamByName('current_user_id').AsString := CurrentUserId;
    FDQuery1.Open;

    while not FDQuery1.Eof do
    begin
      FriendInfo := TFriendInfo.Create;
      FriendInfo.UserNo := FDQuery1.FieldByName('friend_userno').AsInteger;
      FriendInfo.UserId := FDQuery1.FieldByName('friend_id').AsString;

      // 여러 방법으로 시도
      NameStr := FDQuery1.FieldByName('friend_name').AsString;

      // AsString으로 직접 가져오기 (가장 안전)
      FriendInfo.UserName := NameStr;

      FriendInfo.IsSelected := False;
      FAllFriends.Add(FriendInfo);

      FDQuery1.Next;
    end;

    FDQuery1.Close;
  except
    on E: Exception do
      ShowMessage('친구 목록을 불러오는 중 오류 발생: ' + E.Message);
  end;
end;

{ ---------------- Filter ---------------- }
procedure TForm13.FilterList(const Keyword: string);
var
  i: Integer;
  Friend: TFriendInfo;
begin
  lbFriends.Items.BeginUpdate;
  try
    lbFriends.Items.Clear;
    FFilteredFriends.Clear;

    for i := 0 to FAllFriends.Count - 1 do
    begin
      Friend := FAllFriends[i];
      if (Keyword = '') or (Pos(LowerCase(Keyword), LowerCase(Friend.UserName)) > 0) then
      begin
        lbFriends.Items.Add(Friend.UserName);
        FFilteredFriends.Add(Friend);
      end;
    end;

    lbFriends.ItemIndex := -1;
  finally
    lbFriends.Items.EndUpdate;
  end;

  lbFriends.Invalidate;
  userCountLabel.Caption := '친구 ' + IntToStr(FFilteredFriends.Count) + '명';
end;

procedure TForm13.edtSearchChange(Sender: TObject);
begin
  FilterList(Trim(edtSearch.Text));
end;

procedure TForm13.edtSearchEnter(Sender: TObject);
begin
  pbSearchBG.Invalidate;
end;

procedure TForm13.edtSearchExit(Sender: TObject);
begin
  pbSearchBG.Invalidate;
end;

{ ---------------- PaintBox: search bar ---------------- }
procedure TForm13.pbSearchBGPaint(Sender: TObject);
var
  C: TCanvas;
  R: TRect;
  PenCol: TColor;
begin
  C := pbSearchBG.Canvas;
  R := pbSearchBG.ClientRect;

  if edtSearch.Focused then
    PenCol := RGB(100,160,255)
  else
    PenCol := $00C8C8C8;

  C.Brush.Style := bsSolid;
  C.Brush.Color := clWhite;
  C.Pen.Style := psSolid;
  C.Pen.Color := PenCol;
  C.Pen.Width := 1;

  InflateRect(R, -1, -1);

  C.RoundRect(
    R.Left, R.Top, R.Right, R.Bottom,
    R.Height, R.Height
  );

  DrawMagnifier(C, Rect(
    R.Left + 8,
    R.Top + (R.Height - 20) div 2,
    R.Left + 8 + 20,
    R.Top + (R.Height - 20) div 2 + 20
  ));
end;

procedure TForm13.DrawMagnifier(const C: TCanvas; const R: TRect);
begin
  // 검색 아이콘 생략
end;

procedure TForm13.lbFriendsMeasureItem(Control: TWinControl; Index: Integer;
  var Height: Integer);
begin
  Height := Max(AVATAR_SIZE + ITEM_PADDING, 46);
end;

procedure TForm13.lbFriendsDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  C: TCanvas;
  sName: String;  // WideString -> String 변경
  isSel: Boolean;
  YCenter: Integer;
  avatarRect, checkboxRect, R: TRect;
  colBg: TColor;
  TextLeft: Integer;
  Friend: TFriendInfo;
begin
  if (Index < 0) or (Index >= FFilteredFriends.Count) then
    Exit;

  C := lbFriends.Canvas;
  Friend := FFilteredFriends[Index];
  sName := Friend.UserName;
  isSel := Friend.IsSelected;

  if odSelected in State then
    C.Brush.Color := $00F0F8FF
  else
    C.Brush.Color := clWhite;
  C.FillRect(Rect);

  YCenter := (Rect.Top + Rect.Bottom) div 2;

  avatarRect.Left   := Rect.Left + ITEM_PADDING;
  avatarRect.Right  := avatarRect.Left + AVATAR_SIZE;
  avatarRect.Top    := YCenter - AVATAR_SIZE div 2;
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

  R := Rect;
  R.Left := TextLeft;
  R.Right := Rect.Right - 40;

  C.Font.Name := 'Malgun Gothic';
  C.Font.Size := 11;
  C.Font.Style := [];
  C.Font.Color := clWindowText;

  // DrawTextW 대신 TextOut 사용
  C.Brush.Style := bsClear;
  TextOut(C.Handle,
          R.Left,
          R.Top + (R.Bottom - R.Top - C.TextHeight(sName)) div 2,
          PChar(sName), Length(sName));

  // 체크박스
  checkboxRect.Left := Rect.Right - ITEM_PADDING - 20;
  checkboxRect.Right := checkboxRect.Left + 20;
  checkboxRect.Top := YCenter - 10;
  checkboxRect.Bottom := checkboxRect.Top + 20;

  if isSel then
  begin
    C.Brush.Style := bsSolid;
    C.Brush.Color := RGB(100,160,255);
    C.Pen.Color := RGB(100,160,255);
  end
  else
  begin
    C.Brush.Style := bsClear;
    C.Pen.Color := $00C8C8C8;
  end;

  C.RoundRect(checkboxRect.Left, checkboxRect.Top,
              checkboxRect.Right, checkboxRect.Bottom, 4, 4);

  if isSel then
  begin
    C.Pen.Color := clWhite;
    C.Pen.Width := 2;
    C.MoveTo(checkboxRect.Left + 4, checkboxRect.Top + 10);
    C.LineTo(checkboxRect.Left + 8, checkboxRect.Top + 14);
    C.LineTo(checkboxRect.Left + 16, checkboxRect.Top + 6);
    C.Pen.Width := 1;
  end;
end;

procedure TForm13.lbFriendsClick(Sender: TObject);
var
  Index: Integer;
  Friend: TFriendInfo;
begin
  Index := lbFriends.ItemIndex;
  if (Index >= 0) and (Index < FFilteredFriends.Count) then
  begin
    Friend := FFilteredFriends[Index];
    Friend.IsSelected := not Friend.IsSelected;  // 선택 상태 토글
    lbFriends.Invalidate;
  end;
end;

function TForm13.GetSelectedFriend: TFriendInfo;
var
  SelectedIndex: Integer;
begin
  Result := nil;
  SelectedIndex := lbFriends.ItemIndex;

  if (SelectedIndex >= 0) and (SelectedIndex < FFilteredFriends.Count) then
    Result := FFilteredFriends[SelectedIndex];
end;

// 다중 선택된 친구들을 반환하는 새 함수
function TForm13.GetSelectedFriends: TList<TFriendInfo>;
var
  i: Integer;
begin
  Result := TList<TFriendInfo>.Create;

  for i := 0 to FAllFriends.Count - 1 do
  begin
    if FAllFriends[i].IsSelected then
      Result.Add(FAllFriends[i]);
  end;
end;

{ ---------------- 확인 버튼: 채팅방 생성 및 멤버 초대 ---------------- }
procedure TForm13.btnCancelClick(Sender: TObject);
begin
  edtSearch.Clear;
  close;
end;

procedure TForm13.btnOKClick(Sender: TObject);
type
  TLetters = array[0..25] of String;
const
  LETTERS: TLetters = (
    'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p',
    'q','r','s','t','u','v','w','x','y','z'
  );
var
  chatpw, RandChar, chatroomname, InvitedNames: WideString;
  I, R, ChatRoomID, userno, ChatType: Integer;
  SelectedFriends: TList<TFriendInfo>;
  Friend: TFriendInfo;
begin
  SelectedFriends := GetSelectedFriends;
  try
    // 친구 선택 확인
    if SelectedFriends.Count = 0 then
    begin
      ShowMessage('최소 1명 이상의 친구를 선택해주세요.');
      Exit;
    end;

    ChatType := Form8.ChatType;
    chatroomname := Form8.chatroomname;
    userno := CurrentUser.UserNo;

    // 랜덤 비밀번호 생성
    Randomize;
    chatpw := '';

    if ChatType = 1 then
    begin
      for I := 1 to 8 do
      begin
        if Random(2) = 0 then
        begin
          R := Random(26);
          RandChar := LETTERS[R];
        end
        else
        begin
          R := Random(10);
          RandChar := IntToStr(R);
        end;
        chatpw := chatpw + RandChar;
      end;
    end;

    try
      FDConnection1.StartTransaction;

      try
        // 1. 채팅방 생성 (num을 선택된 친구 수 + 1로 설정)
        if ChatType = 1 then
        begin
          FDQuery1.SQL.Text :=
            'INSERT INTO chat(chatroomname, ChatType, chatpw, num) ' +
            'VALUES(:chatroomname, :ChatType, :chatpw, :num) ';
          FDQuery1.ParamByName('chatroomname').AsWideString := chatroomname;
          FDQuery1.ParamByName('chatpw').AsString := chatpw;
          FDQuery1.ParamByName('ChatType').AsInteger := ChatType;
          FDQuery1.ParamByName('num').AsInteger := SelectedFriends.Count + 1;
          FDQuery1.ExecSQL;
        end
        else if ChatType = 2 then
        begin
          FDQuery1.SQL.Text :=
            'INSERT INTO chat(chatroomname, ChatType, num) ' +
            'VALUES(:chatroomname, :ChatType, :num) ';
          FDQuery1.ParamByName('chatroomname').AsWideString := chatroomname;
          FDQuery1.ParamByName('ChatType').AsInteger := ChatType;
          FDQuery1.ParamByName('num').AsInteger := SelectedFriends.Count + 1;
          FDQuery1.ExecSQL;
        end;

        // 2. 생성된 채팅방 ID 가져오기
        FDQuery1.SQL.Text :=
          'SELECT ChatRoomID FROM chat WHERE chatroomname = :chatroomname ' +
          'ORDER BY ChatRoomID DESC LIMIT 1';
        FDQuery1.ParamByName('chatroomname').AsWideString := chatroomname;
        FDQuery1.Open;

        if FDQuery1.RecordCount = 0 then
          raise Exception.Create('채팅방 생성에 실패했습니다.');

        ChatRoomID := FDQuery1.FieldByName('ChatRoomID').AsInteger;
        FDQuery1.Close;

        // 3. chat_user 테이블에 방 생성자 추가
        FDQuery1.SQL.Text :=
          'INSERT INTO chat_user(ChatRoomId, UserNo) ' +
          'VALUES(:ChatRoomId, :UserNo)';
        FDQuery1.ParamByName('ChatRoomId').AsInteger := ChatRoomID;
        FDQuery1.ParamByName('UserNo').AsInteger := userno;
        FDQuery1.ExecSQL;

        // 4. chat_user 테이블에 선택된 모든 친구 추가
        InvitedNames := '';
        for I := 0 to SelectedFriends.Count - 1 do
        begin
          Friend := SelectedFriends[I];

          FDQuery1.SQL.Text :=
            'INSERT INTO chat_user(ChatRoomId, UserNo) ' +
            'VALUES(:ChatRoomId, :UserNo)';
          FDQuery1.ParamByName('ChatRoomId').AsInteger := ChatRoomID;
          FDQuery1.ParamByName('UserNo').AsInteger := Friend.UserNo;
          FDQuery1.ExecSQL;

          // 초대된 친구 이름 목록 생성
          if InvitedNames <> '' then
            InvitedNames := InvitedNames + ', ';
          InvitedNames := InvitedNames + Friend.UserName;
        end;

        FDConnection1.Commit;

        // 5. 채팅방 초기화
        Form1.InitializeChat(ChatRoomID, 0, userno, CurrentUser.Name, chatroomname);

        // 6. 성공 메시지
        if ChatType = 1 then
          ShowMessage('채팅방이 생성되었습니다!' + #13#10 +
                      '방 번호: ' + IntToStr(ChatRoomID) + #13#10 +
                      '방 비밀번호: ' + chatpw + #13#10 +
                      '참여 인원: ' + IntToStr(SelectedFriends.Count + 1) + '명' + #13#10 +
                      '초대된 친구: ' + InvitedNames)
        else
          ShowMessage('채팅방이 생성되었습니다!' + #13#10 +
                      '방 번호: ' + IntToStr(ChatRoomID) + #13#10 +
                      '참여 인원: ' + IntToStr(SelectedFriends.Count + 1) + '명' + #13#10 +
                      '초대된 친구: ' + InvitedNames);


        Form8.Close;
        Form1.Show;
        edtSearch.Clear;
        Self.Close;


      except
        on E: Exception do
        begin
          FDConnection1.Rollback;
          ShowMessage('채팅방 생성 중 오류 발생: ' + E.Message);
        end;
      end;

    except
      on E: Exception do
        ShowMessage('데이터베이스 오류: ' + E.Message);
    end;

  finally
    SelectedFriends.Free;
  end;
end;

end.
