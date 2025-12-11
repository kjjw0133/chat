unit Unit11;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, FireDAC.Stan.Async, FireDAC.DApt,
  FireDAC.UI.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Phys,
  FireDAC.Phys.MySQL, FireDAC.Phys.MySQLDef, FireDAC.VCLUI.Wait, Data.DB,
  FireDAC.Comp.Client, FireDAC.Comp.DataSet,System.Hash;

type
  TForm11 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Edit1: TEdit;
    CheckBox1: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    FDQueryMembers: TFDQuery;
    FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink;
    Label4: TLabel;
    Label5: TLabel;
    FDConnection1: TFDConnection;
    Label6: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form11: TForm11;

implementation

{$R *.dfm}
uses Unit2,unit4,unit10;

procedure TForm11.Button1Click(Sender: TObject);
begin
Form11.Close;
Form4.Show;
end;

procedure TForm11.CheckBox1Click(Sender: TObject);
begin
  if checkbox1.Checked then
  begin
    Edit1.PasswordChar := #0;
  end
  else
  begin
    Edit1.PasswordChar := '*';
  end;
end;

procedure TForm11.FormCreate(Sender: TObject);
begin
  FDQueryMembers.Close;
  FDQueryMembers.SQL.Text := 'SELECT name, id FROM user WHERE userno = :userno ';
  FDQueryMembers.ParamByName('userno').AsInteger := CurrentUser.UserNo;
  FDQueryMembers.Open;

  if not FDQueryMembers.IsEmpty then
  begin
    Label1.Caption := FDQueryMembers.FieldByName('name').AsString;
    Label2.Caption := FDQueryMembers.FieldByName('id').AsString;
  end;

end;

procedure TForm11.Button2Click(Sender: TObject);
var
  pw, hashedInput: String;
begin
  pw := trim(Edit1.Text);
  hashedInput := THashSHA2.GetHashString(pw);
  FDQueryMembers.Close;
  FDQueryMembers.SQL.Text := 'SELECT pw FROM user WHERE userno = :userno ';
  FDQueryMembers.ParamByName('userno').AsInteger := CurrentUser.UserNo;
  FDQueryMembers.Open;

  if not FDQueryMembers.IsEmpty then
    pw := FDQueryMembers.FieldByName('pw').AsString
  else
  begin
    ShowMessage('사용자 정보를 찾을 수 없습니다.');
    Exit;
  end;


  if pw = hashedInput then
  begin
    Application.CreateForm(TForm10, Form10);
    Form11.Close;
    Form10.Show;
  end
  else
  begin
    ShowMessage('비밀번호가 일치하지 않습니다.');
    Exit;
  end;
end;

end.




