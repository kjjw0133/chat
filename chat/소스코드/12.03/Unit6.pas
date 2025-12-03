unit Unit6;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Stan.Async, FireDAC.DApt, FireDAC.UI.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Phys, FireDAC.Phys.MySQL,
  FireDAC.Phys.MySQLDef, FireDAC.VCLUI.Wait, Data.DB, FireDAC.Comp.Client,
  FireDAC.Comp.DataSet, Vcl.StdCtrls,System.Hash;

type
  TForm6 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    FDQueryMembers: TFDQuery;
    Label3: TLabel;
    Edit2: TEdit;
    FDConnection1: TFDConnection;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form6: TForm6;

implementation
uses Unit2, Unit4;

{$R *.dfm}

procedure TForm6.Button1Click(Sender: TObject);
var
pw , id, email, checkpw,hashedInput : String;
begin
  pw := Edit1.Text;
  checkpw := Edit2.Text;
  id := Form4.Edit1.Text;
  email := Form4.Edit2.Text;
  hashedInput := THashSHA2.GetHashString(PW);

  if pw = checkpw then
  begin
    FDQueryMembers.Close;
    FDQueryMembers.SQL.Text := ' update user set pw = :pw where id = :id and email = :email ';
    FDQueryMembers.ParamByName('pw').AsString := hashedInput;
    FDQueryMembers.ParamByName('id').AsString := id;
    FDQueryMembers.ParamByName('email').AsString := email;
    FDQueryMembers.ExecSQL;


  end
  else
  begin
    ShowMessage('비밀번호가 동일하지 않습니다. 다시 확인해주세요.');
    Exit;
  end;
  Form6.Close;
  Form2.Show;
end;

end.
