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
    UserName: String;
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
begin
  FAllFriends.Clear;

  // CurrentUser.UserNo로 user.id를 조회해야 함
  try
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
      FriendInfo.UserName := FDQuery1.FieldByName('friend_name').AsString;
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
  sName: string;
  isSel: Boolean;
  YCenter: Integer;
  avatarRect, radioRect, R: TRect;
  colBg: TColor;
  TextLeft: Integer;
begin
  C := lbFriends.Canvas;
  sName := lbFriends.Items[Index];
  isSel := (lbFriends.ItemIndex = Index);

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

  colBg := RGB(
    150 + (Ord(sName[1]) * 3) mod 100,
    120 + (Ord(sName[1]) * 5) mod 100,
    160 + (Ord(sName[1]) * 7) mod 80
  );

  C.Brush.Color := colBg;
  C.Pen.Color := colBg;
  C.Ellipse(avatarRect.Left, avatarRect.Top, avatarRect.Right, avatarRect.Bottom);

  C.Font.Name := 'Segoe UI';
  C.Font.Size := 10;
  C.Font.Style := [fsBold];
  C.Font.Color := clWhite;

  DrawText(C.Handle, PChar(Copy(sName,1,1)), 1,
    avatarRect, DT_CENTER or DT_VCENTER or DT_SINGLELINE);

  TextLeft := avatarRect.Right + ITEM_PADDING;

  R := Rect;
  R.Left := TextLeft;
  R.Right := Rect.Right - 40;

  C.Font.Size := 11;
  C.Font.Style := [];
  C.Font.Color := clWindowText;

  DrawText(C.Handle, PChar(sName), Length(sName), R,
    DT_LEFT or DT_VCENTER or DT_SINGLELINE);

  radioRect.Left := Rect.Right - ITEM_PADDING - 18;
  radioRect.Right := radioRect.Left + 18;
  radioRect.Top := YCenter - 9;
  radioRect.Bottom := radioRect.Top + 18;

  C.Brush.Style := bsClear;
  C.Pen.Color := $00C8C8C8;
  C.Ellipse(radioRect.Left, radioRect.Top, radioRect.Right, radioRect.Bottom);

  if isSel then
  begin
    C.Brush.Style := bsSolid;
    C.Brush.Color := RGB(100,160,255);
    C.Pen.Color := C.Brush.Color;
    C.Ellipse(radioRect.Left+5, radioRect.Top+5,
              radioRect.Right-5, radioRect.Bottom-5);
  end;
end;

procedure TForm13.lbFriendsClick(Sender: TObject);
begin
  lbFriends.Invalidate;
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

{ ---------------- 확인 버튼: 채팅방 생성 및 멤버 초대 ---------------- }
procedure TForm13.btnCancelClick(Sender: TObject);
begin
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
  chatpw, RandChar, chatroomname: String;
  I, R, ChatRoomID, userno, ChatType: Integer;
  SelectedFriend: TFriendInfo;
begin
  // 친구 선택 확인
  if lbFriends.ItemIndex < 0 then
  begin
    ShowMessage('초대할 친구를 선택해주세요.');
    Exit;
  end;

  ChatType := Form8.ChatType;
  chatroomname := Form8.chatroomname;
  userno := CurrentUser.UserNo;
  SelectedFriend := GetSelectedFriend;

  if SelectedFriend = nil then
  begin
    ShowMessage('선택한 친구 정보를 가져올 수 없습니다.');
    Exit;
  end;

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
      // 1. 채팅방 생성
      if ChatType = 1 then
      begin
        FDQuery1.SQL.Text :=
          'INSERT INTO chat(chatroomname, ChatType, chatpw, num) ' +
          'VALUES(:chatroomname, :ChatType, :chatpw, 2)';
        FDQuery1.ParamByName('chatroomname').AsWideString := chatroomname;
        FDQuery1.ParamByName('chatpw').AsString := chatpw;
        FDQuery1.ParamByName('ChatType').AsInteger := ChatType;
        FDQuery1.ExecSQL;
      end
      else if ChatType = 2 then
      begin
        FDQuery1.SQL.Text :=
          'INSERT INTO chat(chatroomname, ChatType, num) ' +
          'VALUES(:chatroomname, :ChatType, 2)';
        FDQuery1.ParamByName('chatroomname').AsWideString := chatroomname;
        FDQuery1.ParamByName('ChatType').AsInteger := ChatType;
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

      // 4. chat_user 테이블에 초대된 친구 추가
      FDQuery1.SQL.Text :=
        'INSERT INTO chat_user(ChatRoomId, UserNo) ' +
        'VALUES(:ChatRoomId, :UserNo)';
      FDQuery1.ParamByName('ChatRoomId').AsInteger := ChatRoomID;
      FDQuery1.ParamByName('UserNo').AsInteger := SelectedFriend.UserNo;
      FDQuery1.ExecSQL;

      FDConnection1.Commit;

      // 5. 채팅방 초기화
      Form1.InitializeChat(ChatRoomID, 0, userno, CurrentUser.Name, chatroomname);

      // 6. 성공 메시지
      if ChatType = 1 then
        ShowMessage('채팅방이 생성되었습니다!' + #13#10 +
                    '방 번호: ' + IntToStr(ChatRoomID) + #13#10 +
                    '방 비밀번호: ' + chatpw + #13#10 +
                    '초대된 친구: ' + SelectedFriend.UserName)
      else
        ShowMessage('채팅방이 생성되었습니다!' + #13#10 +
                    '방 번호: ' + IntToStr(ChatRoomID) + #13#10 +
                    '초대된 친구: ' + SelectedFriend.UserName);

      Form8.Close;
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
end;

end.
