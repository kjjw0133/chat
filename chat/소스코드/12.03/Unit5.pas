unit Unit5;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, FireDAC.Stan.Async, FireDAC.DApt,
  FireDAC.UI.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Phys,
  FireDAC.Phys.MySQL, FireDAC.Phys.MySQLDef, FireDAC.VCLUI.Wait, Data.DB,
  FireDAC.Comp.Client, FireDAC.Comp.DataSet;

type
  TForm5 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    FDQueryMembers: TFDQuery;
    FDConnection1: TFDConnection;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form5: TForm5;
  IsLoggedIn: Boolean = False;

implementation
uses Unit3, Unit1, Unit4, Unit6;

{$R *.dfm}

procedure TForm5.Button1Click(Sender: TObject);
var
pw,email,id : String;

begin
  email := form4.EMAIL;
  id := form4.ID;
  pw := '';

  FDQueryMembers.Close;
  FDQueryMembers.SQL.Text := ' select pw from user where email = :email and id = :id';
  FDQueryMembers.ParamByName('email').AsString := email;
  FDQueryMembers.ParamByName('id').AsString := id;
  FDQueryMembers.Open;

  pw := FDQueryMembers.FieldByName('pw').AsString;


  if not FDQueryMembers.IsEmpty then
  begin
    if pw = Edit1.Text then
    begin
        IsLoggedIn := True;
        ShowMessage('로그인 성공');

        Form5.Close;
        Form6.Show;
    end
    else
    begin
      ShowMessage('오류 발생');
    end;

  end;

end;

end.
