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
   procedure PanelRecommendClick(Sender: TObject);
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
    panel.OnClick := PanelRecommendClick; 

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

    FDQuery1.Next;
    VerticalOffset := VerticalOffset + panel.Height + 4;
  end;

end;

procedure TForm16.PanelRecommendClick(Sender: TObject);
var
  pnl: TPanel;
  recommendedUserNo: Integer;
begin
  if Sender is TPanel then
  begin
    pnl := TPanel(Sender);
    recommendedUserNo := pnl.Tag; 
    ShowMessage('추천 사용자를 추가합니다. UserNo = ' + IntToStr(recommendedUserNo));
  end;
end;


end.


