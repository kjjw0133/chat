unit Unit16;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.Phys.MySQL, FireDAC.Phys.MySQLDef, FireDAC.VCLUI.Wait,
  Data.DB, FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.Comp.DataSet;

type
  TForm16 = class(TForm)
    Edit1: TEdit;
    Label1: TLabel;
    Panel1: TPanel;
    FDConnection1: TFDConnection;
    FDQuery1: TFDQuery;
    Button1: TButton;
    Button2: TButton;
    FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink;
    ScrollBox1: TScrollBox;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
   procedure AcceptButtonClick(Sender: TObject);
   procedure RejectButtonClick(Sender: TObject);
  public
    { Public declarations }
  end;

var
  Form16: TForm16;

implementation

{$R *.dfm}
uses unit2,unit14;

procedure TForm16.Button1Click(Sender: TObject);
var
  requester_id, receiver_id : String;
begin
  requester_id := CurrentUser.ID;
  receiver_id := Trim(Edit1.Text);

  FDQuery1.Close;
  FDQuery1.SQL.Text := 'SELECT id FROM `user` WHERE id = :id';
  FDQuery1.ParamByName('id').AsWideString := receiver_id;
  FDQuery1.Open;

  if FDQuery1.IsEmpty then
  begin
    ShowMessage('해당 사용자가 존재하지 않습니다.');
    Exit;
  end;

  FDQuery1.Close;
  FDQuery1.SQL.Text := ' SELECT requester_id, receiver_id '+
                       ' FROM friend '+
                       ' WHERE status IN (1, 2) '+
                       ' AND ( (requester_id = :me AND receiver_id = :other) '+
                       ' OR (requester_id = :other AND receiver_id = :me) ) ';

  FDQuery1.ParamByName('me').AsWideString := requester_id;
  FDQuery1.ParamByName('other').AsWideString := receiver_id;
  FDQuery1.Open;

   if not FDQuery1.IsEmpty then
   begin
    ShowMessage('이미 초대하거나 친구 상태입니다.');
    Exit;
   end;

  FDQuery1.Close;
  FDQuery1.SQL.Text := ' INSERT INTO friend (requester_id, receiver_id, status) '+
                       ' SELECT u1.id, u2.id, 1 '+
                       ' FROM `user` u1 '+
                       ' JOIN `user` u2 '+
                       ' WHERE u1.id = :requester_id '+
                       ' AND u2.id = :receiver_id ';
  FDQuery1.ParamByName('requester_id').AsWideString := requester_id;
  FDQuery1.ParamByName('receiver_id').AsWideString := receiver_id;
  FDQuery1.ExecSQL;
  ShowMessage('친구 추가 요청이 성공적으로 완료되었습니다.');
  Form16.Close;

  TForm14.Create(Application);

end;

procedure TForm16.Button2Click(Sender: TObject);
begin
  Edit1.Clear;
  Form16.Close;

  TForm14.Create(Application);
end;

procedure TForm16.FormCreate(Sender: TObject);
var
  panel: TPanel;
  myno: Integer;
  usernoLabel, usernameLabel, useridLabel: TLabel;
  AcceptBtn, RejectBtn: TButton;
  VerticalOffset: Integer;
begin
  myno := CurrentUser.UserNo;
  ActiveControl := Panel1;
  Panel1.Color := clWhite;

  FDQuery1.Close;
  FDQuery1.SQL.Text :=
    ' SELECT DISTINCT cu2.UserNo AS recommended_userno, ' +
    ' u.name AS recommended_username, ' +
    ' u.id AS recommended_userid ' +
    ' FROM chat_user cu JOIN chat_user cu2 ' +
    '   ON cu.ChatRoomId = cu2.ChatRoomId ' +
    ' JOIN `user` u ON cu2.UserNo = u.userno ' +
    ' LEFT JOIN friend f ' +
    '   ON ( (f.requester_id = (SELECT id FROM `user` WHERE userno = :myno) AND f.receiver_id = u.id) ' +
    '     OR (f.receiver_id = (SELECT id FROM `user` WHERE userno = :myno) AND f.requester_id = u.id) ) ' +
    ' WHERE cu.UserNo = :myno AND cu2.UserNo != :myno ' +
    '   AND (f.status IS NULL OR (f.status != 2 AND f.status != 1))';

  FDQuery1.ParamByName('myno').AsInteger := myno;
  FDQuery1.Open;

  VerticalOffset := 0;
  FDQuery1.First;
  while not FDQuery1.Eof do
  begin
    panel := TPanel.Create(ScrollBox1);
    panel.Parent := ScrollBox1;
    panel.Align := alTop;
    panel.Height := 60;
    panel.Caption := '';
    panel.Top := VerticalOffset;
    panel.Tag := FDQuery1.FieldByName('recommended_userno').AsInteger;

    usernoLabel := TLabel.Create(panel);
    usernoLabel.Parent := panel;
    usernoLabel.Left := 8;
    usernoLabel.Top := 8;
    usernoLabel.Caption := IntToStr(FDQuery1.FieldByName('recommended_userno').AsInteger);

    usernameLabel := TLabel.Create(panel);
    usernameLabel.Parent := panel;
    usernameLabel.Left := 80;
    usernameLabel.Top := 8;
    usernameLabel.Caption := FDQuery1.FieldByName('recommended_username').AsString;

    useridLabel := TLabel.Create(panel);
    useridLabel.Parent := panel;
    useridLabel.Left := 80;
    useridLabel.Top := 30;
    useridLabel.Caption := FDQuery1.FieldByName('recommended_userid').AsString;

     AcceptBtn := TButton.Create(Panel);
      AcceptBtn.Parent := Panel;
      AcceptBtn.Left := Panel.Width - 180;
      AcceptBtn.Top := 20;
      AcceptBtn.Width := 70;
      AcceptBtn.Height := 30;
      AcceptBtn.Caption := '친구 추가';
      AcceptBtn.Font.Name := 'Malgun Gothic';
      AcceptBtn.Font.Size := 9;
      AcceptBtn.Anchors := [akTop, akRight];
      AcceptBtn.Tag :=  FDQuery1.FieldByName('recommended_userno').AsInteger;
      AcceptBtn.OnClick := AcceptButtonClick;

      RejectBtn := TButton.Create(Panel);
      RejectBtn.Parent := Panel;
      RejectBtn.Left := Panel.Width - 100;
      RejectBtn.Top := 20;
      RejectBtn.Width := 70;
      RejectBtn.Height := 30;
      RejectBtn.Caption := 'x';
      RejectBtn.Font.Name := 'Malgun Gothic';
      RejectBtn.Font.Size := 9;
      RejectBtn.Anchors := [akTop, akRight];
      RejectBtn.Tag := FDQuery1.FieldByName('recommended_userno').AsInteger;
      RejectBtn.OnClick := RejectButtonClick;

    FDQuery1.Next;
    VerticalOffset := VerticalOffset + panel.Height + 4;
  end;

  FDQuery1.Close;
end;

procedure TForm16.AcceptButtonClick(Sender: TObject);
var
  selectedUserNo: Integer;
  requester_id, receiver_id: String;
begin
  if Sender is TButton then
  begin
    selectedUserNo := TButton(Sender).Tag;
    requester_id := CurrentUser.ID;

    // FDQuery1을 재사용 (이미 폼에 생성됨)
    FDQuery1.Close;
    FDQuery1.SQL.Text := 'SELECT id FROM `user` WHERE userno = :userno';
    FDQuery1.ParamByName('userno').AsInteger := selectedUserNo;
    FDQuery1.Open;

    if not FDQuery1.IsEmpty then
    begin
      receiver_id := FDQuery1.FieldByName('id').AsWideString;

      FDQuery1.Close;
      FDQuery1.SQL.Text := 'INSERT INTO friend (requester_id, receiver_id, status) VALUES (:requester_id, :receiver_id, 1)';
      FDQuery1.ParamByName('requester_id').AsWideString := requester_id;
      FDQuery1.ParamByName('receiver_id').AsWideString := receiver_id;
      FDQuery1.ExecSQL;

      ShowMessage('친구 추가 요청이 완료되었습니다.');

      // 추가된 패널 제거
      if Sender is TButton then
        TButton(Sender).Parent.Free;
    end
    else
    begin
      ShowMessage('사용자를 찾을 수 없습니다.');
    end;

    FDQuery1.Close;
  end;
end;

procedure TForm16.RejectButtonClick(Sender: TObject);
var
  selectedUserNo : Integer;
begin
  if Sender is TButton then
  begin
    selectedUserNo := TButton(Sender).Tag;

    TButton(Sender).Parent.Free;
  end;
end;

end.

