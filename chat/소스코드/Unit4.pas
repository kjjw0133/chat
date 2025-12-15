unit Unit4;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, FireDAC.Stan.Async, FireDAC.DApt,
  FireDAC.UI.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Phys,
  FireDAC.Phys.MySQL, FireDAC.Phys.MySQLDef, FireDAC.VCLUI.Wait, Data.DB,
  FireDAC.Comp.Client, FireDAC.Comp.DataSet ,System.Hash, IdIOHandler,
  IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSL, IdComponent,
  IdTCPConnection, IdTCPClient, IdExplicitTLSClientServerBase, IdMessageClient,
  IdSMTPBase, IdSMTP, IdBaseComponent, IdMessage;

type
  TForm4 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    FDQueryMembers: TFDQuery;
    IdMessage1: TIdMessage;
    IdSMTP1: TIdSMTP;
    IdSSLIOHandlerSocketOpenSSL1: TIdSSLIOHandlerSocketOpenSSL;
    FDConnection1: TFDConnection;
    Label3: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    ID : String;
    EMAIL : String;
  end;

var
  Form4: TForm4;

implementation

uses Unit5;
{$R *.dfm}

procedure TForm4.Button1Click(Sender: TObject);
type
  TLetters = array[0..25] of String;
const
  LETTERS: TLetters = (
    'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p',
    'q','r','s','t','u','v','w','x','y','z'
  );
var
//  ID, EMAIL,
  PW, RandChar,hashedInput: String;
  I, R: Integer;
  Msg: TIdMessage;
  SMTP: TIdSMTP;
  SSL: TIdSSLIOHandlerSocketOpenSSL;
begin
  ID := Trim(Edit1.Text);
  EMAIL := Trim(Edit2.Text);
  Randomize;
  PW := '';

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
    PW := PW + RandChar;
  end;

  FDQueryMembers.Close;
  FDQUeryMembers.SQL.Text := 'select email, id from user where id = :id and email = :email ';
  FDQueryMembers.ParamByName('id').AsString := ID;
  FDQueryMembers.ParamByName('email').AsString := EMAIL;
  FDQueryMembers.Open;

  if Trim(Edit2.Text) = FDQueryMembers.FieldByName('email').AsString then
  begin
    if Trim(Edit1.Text) = FDQueryMembers.FieldByName('id').AsString then
    begin
      hashedInput := THashSHA2.GetHashString(PW);
      FDQueryMembers.Close;
      FDQueryMembers.SQL.Text :=
        'UPDATE user SET pw = :pw WHERE id = :id AND email = :email';

      FDQueryMembers.ParamByName('pw').AsString := hashedInput;
      FDQueryMembers.ParamByName('id').AsString := ID;
      FDQueryMembers.ParamByName('email').AsString := EMAIL;
      FDQueryMembers.ExecSQL;
      ShowMessage('비밀번호 :'+ hashedInput);
    end
    else
    begin
      ShowMessage('아이디가 일치하지 않습니다.');
      Exit;
    end;
  end
  else
  begin
    ShowMessage('이메일이 일치하지 않습니다.');
    Exit;
  end;

SMTP := TIdSMTP.Create(nil);
Msg := TIdMessage.Create(nil);
SSL := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
try
  SSL.SSLOptions.Method := sslvTLSv1_2;
  SSL.SSLOptions.Mode := sslmUnassigned;

  SMTP.IOHandler := SSL;
  SMTP.Host := 'smtp.gmail.com';
  SMTP.Port := 465;
  SMTP.UseTLS := utUseImplicitTLS;
  SMTP.Username := ''; // 보내는 메일
  SMTP.Password := ''; // Gmail 앱 비밀번호 사용!

  Msg.From.Address := SMTP.Username;
  Msg.Recipients.Add.Address := EMAIL;
  Msg.Subject := '임시 비밀번호 전송';

  Msg.CharSet := 'utf-8';
  Msg.ContentType := 'text/plain; charset=UTF-8';
  Msg.Encoding := meMIME;
  Msg.ContentType := 'text/html';

  Msg.Body.Text :=
    '<br><b style= " font-size: 30px;">아래의 인증번호를 입력하세요.</b></br>' + sLineBreak +
    '<h1 style= " font-size: 30px;">'+ PW + '</h1>' + sLineBreak + sLineBreak +
    '<b style= " font-size: 30px;">감사합니다.</b>';

  try
    SMTP.Connect;
    SMTP.Send(Msg);
    ShowMessage('메일 전송 완료!');
    Form5.Show;
    Form4.Close;
  finally
    SMTP.Disconnect;
  end;
except
  on E: Exception do
    ShowMessage('메일 전송 실패: ' + E.Message);
end;

end;

procedure TForm4.FormCreate(Sender: TObject);
begin
  Edit1.Clear;
  Edit2.Clear;
end;

end.
