unit Unit17;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.Buttons, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Stan.Async, FireDAC.DApt, FireDAC.UI.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Phys, FireDAC.Phys.MySQL,
  FireDAC.Phys.MySQLDef, FireDAC.VCLUI.Wait, Data.DB, FireDAC.Comp.Client,System.Threading,Generics.Collections;

type
  TFriendRequestInfo = record
    RequestID: Integer;
    RequesterID: String;
    RequesterNo: Integer;
    RequesterName: String;
  end;

    TFriendReceiverInfo = record
    RequestID: Integer;
    ReceiverID: String;
    ReceiverNo: Integer;
    ReceiverName: String;
  end;

  TForm17 = class(TForm)
    PanelHeader: TPanel;
    LabelTitle: TLabel;
    ScrollBox1: TScrollBox;
    PanelBottom: TPanel;
    FDConnection1: TFDConnection;
    FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink;
    requestCountLabel: TLabel;
    receiverCountLabel: TLabel;
    ScrollBox2: TScrollBox;
    Panel1: TPanel;
    Label1: TLabel;

    procedure FormCreate(Sender: TObject);
    procedure LabelCloseClick(Sender: TObject);
  private
    procedure LoadFriendRequests; virtual;
    procedure LoadFriendReceiver; virtual;
    procedure AcceptButtonClick(Sender: TObject);
    procedure RejectButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
  public
  end;

var
  Form17: TForm17;

implementation

uses unit2;

{$R *.dfm}

procedure TForm17.FormCreate(Sender: TObject);
begin
  LoadFriendRequests;
  LoadFriendReceiver;
end;

procedure TForm17.LabelCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TForm17.LoadFriendRequests;
var
  Query: TFDQuery;
  CurrentUserId: String;
  RequestPanel: TPanel;
  AvatarShape: TShape;
  NameLabel, InitialLabel: TLabel;
  AcceptBtn, RejectBtn: TButton;
  TopOffset: Integer;
  RequestInfo: TFriendRequestInfo;
  RequestCount: Integer;
  AvatarColor: TColor;
  i: Integer;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1;

    Query.SQL.Text := 'SELECT id FROM user WHERE userno = :userno';
    Query.ParamByName('userno').AsInteger := CurrentUser.UserNo;
    Query.Open;

    if Query.RecordCount = 0 then
    begin
      ShowMessage('사용자 정보를 찾을 수 없습니다.');
      Exit;
    end;

    CurrentUserId := Query.FieldByName('id').AsString;
    Query.Close;

    // ScrollBox1의 기존 컴포넌트 모두 제거
    try
      while ScrollBox1.ControlCount > 0 do
      begin
        if ScrollBox1.Controls[0] <> nil then
          ScrollBox1.Controls[0].Free;
      end;
    except
      // 컴포넌트 제거 중 오류 발생 시 무시
    end;

    Query.SQL.Text :=
      'SELECT f.request_id, ' +
      '       f.requester_id, ' +
      '       u.userno AS requester_no, ' +
      '       u.name AS requester_name ' +
      'FROM friend f ' +
      'JOIN user u ON f.requester_id = u.id ' +
      'WHERE f.receiver_id = :current_user_id ' +
      '  AND f.status = 1 ' +
      'ORDER BY f.request_id DESC';

    Query.ParamByName('current_user_id').AsString := CurrentUserId;
    Query.Open;

    RequestCount := Query.RecordCount;
    requestCountLabel.Caption := '친구 요청 ' + IntToStr(RequestCount) + '건';

    if Query.RecordCount = 0 then
    begin
      RequestPanel := TPanel.Create(ScrollBox1);
      RequestPanel.Parent := ScrollBox1;
      RequestPanel.Align := alTop;
      RequestPanel.Height := 80;
      RequestPanel.Caption := '';
      RequestPanel.BevelOuter := bvNone;
      RequestPanel.Color := clWhite;

      NameLabel := TLabel.Create(RequestPanel);
      NameLabel.Parent := RequestPanel;
      NameLabel.Left := 20;
      NameLabel.Top := 30;
      NameLabel.Caption := '받은 친구 요청이 없습니다.';
      NameLabel.Font.Size := 11;
      NameLabel.Font.Color := clGray;

      Exit;
    end;

    TopOffset := 0;

    while not Query.Eof do
    begin
      RequestInfo.RequestID := Query.FieldByName('request_id').AsInteger;
      RequestInfo.RequesterID := Query.FieldByName('requester_id').AsString;
      RequestInfo.RequesterNo := Query.FieldByName('requester_no').AsInteger;
      RequestInfo.RequesterName := Query.FieldByName('requester_name').AsString;

      RequestPanel := TPanel.Create(ScrollBox1);
      RequestPanel.Parent := ScrollBox1;
      RequestPanel.Align := alTop;
      RequestPanel.Height := 70;
      RequestPanel.Caption := '';
      RequestPanel.BevelOuter := bvNone;
      RequestPanel.Color := clWhite;
      RequestPanel.Top := TopOffset;

      if Length(RequestInfo.RequesterName) > 0 then
      begin
        AvatarColor := RGB(
          150 + (Ord(RequestInfo.RequesterName[1]) * 3) mod 100,
          120 + (Ord(RequestInfo.RequesterName[1]) * 5) mod 100,
          160 + (Ord(RequestInfo.RequesterName[1]) * 7) mod 80
        );
      end
      else
        AvatarColor := RGB(150, 120, 160);

      AvatarShape := TShape.Create(RequestPanel);
      AvatarShape.Parent := RequestPanel;
      AvatarShape.Left := 15;
      AvatarShape.Top := 15;
      AvatarShape.Width := 40;
      AvatarShape.Height := 40;
      AvatarShape.Shape := stCircle;
      AvatarShape.Brush.Color := AvatarColor;
      AvatarShape.Pen.Style := psClear;

      InitialLabel := TLabel.Create(RequestPanel);
      InitialLabel.Parent := RequestPanel;
      InitialLabel.Left := 25;
      InitialLabel.Top := 20;
      InitialLabel.Caption := Copy(RequestInfo.RequesterName, 1, 1);
      InitialLabel.Font.Name := 'Malgun Gothic';
      InitialLabel.Font.Size := 14;
      InitialLabel.Font.Style := [fsBold];
      InitialLabel.Font.Color := clWhite;
      InitialLabel.Transparent := True;

      NameLabel := TLabel.Create(RequestPanel);
      NameLabel.Parent := RequestPanel;
      NameLabel.Left := 65;
      NameLabel.Top := 25;
      NameLabel.Caption := RequestInfo.RequesterName + '님이 친구 요청을 보냈습니다';
      NameLabel.Font.Name := 'Malgun Gothic';
      NameLabel.Font.Size := 10;
      NameLabel.Font.Color := clBlack;

      AcceptBtn := TButton.Create(RequestPanel);
      AcceptBtn.Parent := RequestPanel;
      AcceptBtn.Left := RequestPanel.Width - 180;
      AcceptBtn.Top := 20;
      AcceptBtn.Width := 70;
      AcceptBtn.Height := 30;
      AcceptBtn.Caption := '수락';
      AcceptBtn.Font.Name := 'Malgun Gothic';
      AcceptBtn.Font.Size := 9;
      AcceptBtn.Anchors := [akTop, akRight];
      AcceptBtn.Tag := RequestInfo.RequestID;
      AcceptBtn.OnClick := AcceptButtonClick;

      RejectBtn := TButton.Create(RequestPanel);
      RejectBtn.Parent := RequestPanel;
      RejectBtn.Left := RequestPanel.Width - 100;
      RejectBtn.Top := 20;
      RejectBtn.Width := 70;
      RejectBtn.Height := 30;
      RejectBtn.Caption := '거절';
      RejectBtn.Font.Name := 'Malgun Gothic';
      RejectBtn.Font.Size := 9;
      RejectBtn.Anchors := [akTop, akRight];
      RejectBtn.Tag := RequestInfo.RequestID;
      RejectBtn.OnClick := RejectButtonClick;

      TopOffset := TopOffset + RequestPanel.Height + 1;
      Query.Next;
    end;

  finally
    Query.Free;
  end;
end;

procedure TForm17.LoadFriendReceiver;
var
  Query: TFDQuery;
  CurrentUserId: String;
  ReceiverPanel: TPanel;
  AvatarShape: TShape;
  NameLabel, InitialLabel: TLabel;
  RejectBtn: TButton;
  TopOffset: Integer;
  ReceiverInfo: TFriendReceiverInfo;
  ReceiverCount: Integer;
  AvatarColor: TColor;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1;

    Query.SQL.Text := 'select id from user where userno = :userno ';
    Query.ParamByName('userno').AsInteger := CurrentUser.UserNo;
    Query.Open;

    if Query.RecordCount = 0 then
    begin
      ShowMessage('사용자 정보를 찾을 수 없습니다.');
      Exit;
    end;

    CurrentUserId := Query.FieldByName('id').AsString;
    Query.Close;

    // ScrollBox2의 기존 컴포넌트 모두 제거
    try
      while ScrollBox2.ControlCount > 0 do
      begin
        if ScrollBox2.Controls[0] <> nil then
          ScrollBox2.Controls[0].Free;
      end;
    except
      // 컴포넌트 제거 중 오류 발생 시 무시
    end;

    Query.SQL.Text := 'SELECT f.request_id, f.receiver_id, '+
    ' u.userno AS receiver_no, u.name AS receiver_name '+
    ' FROM friend f JOIN user u ON f.receiver_id = u.id '+
    ' WHERE f.requester_id = :current_user_id '+
    ' AND f.status = 1 ORDER BY f.request_id DESC ';

    Query.ParamByName('current_user_id').AsString := CurrentUserId;
    Query.Open;

    ReceiverCount := Query.RecordCount;
    receiverCountLabel.Caption := '보낸 친구 요청 ' + IntToStr(ReceiverCount) + '건';

    if Query.RecordCount = 0 then
    begin
      ReceiverPanel := TPanel.Create(ScrollBox2);
      ReceiverPanel.Parent := ScrollBox2;
      ReceiverPanel.Align := alTop;
      ReceiverPanel.Height := 80;
      ReceiverPanel.Caption := '';
      ReceiverPanel.BevelOuter := bvNone;
      ReceiverPanel.Color := clWhite;

      NameLabel := TLabel.Create(ReceiverPanel);
      NameLabel.Parent := ReceiverPanel;
      NameLabel.Left := 20;
      NameLabel.Top := 30;
      NameLabel.Caption := '보낸 친구 요청이 없습니다.';
      NameLabel.Font.Size := 11;
      NameLabel.Font.Color := clGray;

      Exit;
    end;

    TopOffset := 0;

    while not Query.Eof do
    begin
      ReceiverInfo.RequestID := Query.FieldByName('request_id').AsInteger;
      ReceiverInfo.ReceiverID := Query.FieldByName('receiver_id').AsString;
      ReceiverInfo.ReceiverNo := Query.FieldByName('receiver_no').AsInteger;
      ReceiverInfo.ReceiverName := Query.FieldByName('receiver_name').AsString;

      ReceiverPanel := TPanel.Create(ScrollBox2);
      ReceiverPanel.Parent := ScrollBox2;
      ReceiverPanel.Align := alTop;
      ReceiverPanel.Height := 70;
      ReceiverPanel.Caption := '';
      ReceiverPanel.BevelOuter := bvNone;
      ReceiverPanel.Color := clWhite;
      ReceiverPanel.Top := TopOffset;

      if Length(ReceiverInfo.ReceiverName) > 0 then
      begin
        AvatarColor := RGB(
          150 + (Ord(ReceiverInfo.ReceiverName[1]) * 3) mod 100,
          120 + (Ord(ReceiverInfo.ReceiverName[1]) * 5) mod 100,
          160 + (Ord(ReceiverInfo.ReceiverName[1]) * 7) mod 80
        );
      end
      else
        AvatarColor := RGB(150, 120, 160);

      AvatarShape := TShape.Create(ReceiverPanel);
      AvatarShape.Parent := ReceiverPanel;
      AvatarShape.Left := 15;
      AvatarShape.Top := 15;
      AvatarShape.Width := 40;
      AvatarShape.Height := 40;
      AvatarShape.Shape := stCircle;
      AvatarShape.Brush.Color := AvatarColor;
      AvatarShape.Pen.Style := psClear;

      InitialLabel := TLabel.Create(ReceiverPanel);
      InitialLabel.Parent := ReceiverPanel;
      InitialLabel.Left := 25;
      InitialLabel.Top := 20;
      InitialLabel.Caption := Copy(ReceiverInfo.ReceiverName, 1, 1);
      InitialLabel.Font.Name := 'Malgun Gothic';
      InitialLabel.Font.Size := 14;
      InitialLabel.Font.Style := [fsBold];
      InitialLabel.Font.Color := clWhite;
      InitialLabel.Transparent := True;

      NameLabel := TLabel.Create(ReceiverPanel);
      NameLabel.Parent := ReceiverPanel;
      NameLabel.Left := 65;
      NameLabel.Top := 25;
      NameLabel.Caption := ReceiverInfo.ReceiverName + '님에게 친구 요청을 보냈습니다';
      NameLabel.Font.Name := 'Malgun Gothic';
      NameLabel.Font.Size := 10;
      NameLabel.Font.Color := clBlack;

      RejectBtn := TButton.Create(ReceiverPanel);
      RejectBtn.Parent := ReceiverPanel;
      RejectBtn.Left := ReceiverPanel.Width - 100;
      RejectBtn.Top := 20;
      RejectBtn.Width := 70;
      RejectBtn.Height := 30;
      RejectBtn.Caption := '취소';
      RejectBtn.Font.Name := 'Malgun Gothic';
      RejectBtn.Font.Size := 9;
      RejectBtn.Anchors := [akTop, akRight];
      RejectBtn.Tag := ReceiverInfo.RequestID;
      RejectBtn.OnClick := DeleteButtonClick;

      TopOffset := TopOffset + ReceiverPanel.Height + 1;
      Query.Next;
    end;

  finally
    Query.Free;
  end;
end;

procedure TForm17.AcceptButtonClick(Sender: TObject);
var
  RequestID: Integer;
  Query: TFDQuery;
  Btn: TButton;
begin
  Btn := Sender as TButton;
  RequestID := Btn.Tag;

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1;
    Query.SQL.Text :=
      'UPDATE friend SET status = 2, created_at = now() WHERE request_id = :request_id';
    Query.ParamByName('request_id').AsInteger := RequestID;
    Query.ExecSQL;
  finally
    Query.Free;
  end;

  Btn.Enabled := False;

  // 현재 이벤트가 끝난 후 안전하게 UI 갱신 수행
  TThread.Queue(nil,
    procedure
    begin
      ShowMessage('친구 요청을 수락했습니다.');
      // ScrollBox1의 모든 Panel을 지우고 DB에서 다시 조회
      LoadFriendRequests;
    end
  );
end;

procedure TForm17.RejectButtonClick(Sender: TObject);
var
  RequestID: Integer;
  Query: TFDQuery;
  Btn: TButton;
begin
  Btn := Sender as TButton;
  RequestID := Btn.Tag;

  if MessageDlg('친구 요청을 거절하시겠습니까?', mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1;
    Query.SQL.Text :=
      'UPDATE friend SET status = 0, created_at = now() WHERE request_id = :request_id';
    Query.ParamByName('request_id').AsInteger := RequestID;
    Query.ExecSQL;
  finally
    Query.Free;
  end;

  Btn.Enabled := False;

  TThread.Queue(nil,
    procedure
    begin
      ShowMessage('친구 요청을 거절했습니다.');
      LoadFriendRequests;  // ScrollBox1 갱신
//      LoadFriendReceiver;  // ScrollBox2 갱신
    end
  );
end;

procedure TForm17.DeleteButtonClick(Sender: TObject);
var
  RequestID: Integer;
  Query: TFDQuery;
  Btn: TButton;
begin
  Btn := Sender as TButton;
  RequestID := Btn.Tag;

  if MessageDlg('친구 요청을 취소하시겠습니까?', mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1;
    Query.SQL.Text := 'DELETE FROM friend WHERE request_id = :request_id';
    Query.ParamByName('request_id').AsInteger := RequestID;
    Query.ExecSQL;
  finally
    Query.Free;
  end;

  Btn.Enabled := False;

  TThread.Queue(nil,
    procedure
    begin
      ShowMessage('친구 요청을 취소했습니다.');
      LoadFriendReceiver;  // ScrollBox2의 모든 Panel을 지우고 DB에서 다시 조회
      //LoadFriendRequests;  // ScrollBox1도 갱신
    end
  );
end;

end.
