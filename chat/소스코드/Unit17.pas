unit Unit17;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.Buttons, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Stan.Async, FireDAC.DApt, FireDAC.UI.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Phys, FireDAC.Phys.MySQL,
  FireDAC.Phys.MySQLDef, FireDAC.VCLUI.Wait, Data.DB, FireDAC.Comp.Client,System.Threading;

type
  TFriendRequestInfo = record
    RequestID: Integer;
    RequesterID: String;
    RequesterNo: Integer;
    RequesterName: String;
  end;

  TForm17 = class(TForm)
    PanelHeader: TPanel;
    LabelTitle: TLabel;
    LabelClose: TLabel;
    ScrollBox1: TScrollBox;
    PanelBottom: TPanel;
    FDConnection1: TFDConnection;
    FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink;
    requestCountLabel: TLabel;

    procedure FormCreate(Sender: TObject);
    procedure LabelCloseClick(Sender: TObject);
  private
    procedure LoadFriendRequests;
    procedure AcceptButtonClick(Sender: TObject);
    procedure RejectButtonClick(Sender: TObject);
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
begin
  ScrollBox1.DestroyComponents;

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

  //  버튼을 비활성화
  Btn.Enabled := False;

  TThread.Queue(nil,
    procedure
    begin
      ShowMessage('친구 요청을 수락했습니다.');
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
      LoadFriendRequests;
    end
  );
end;

end.

