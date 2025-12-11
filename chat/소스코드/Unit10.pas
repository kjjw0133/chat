unit Unit10;

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
  TForm10 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Button1: TButton;
    Button2: TButton;
    FDQueryMembers: TFDQuery;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    FDConnection1: TFDConnection;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form10: TForm10;

implementation

{$R *.dfm}
uses unit2, unit6,unit7,unit11;

procedure TForm10.Button1Click(Sender: TObject);
begin
  Form6.Show;
end;

procedure TForm10.Button2Click(Sender: TObject);
begin
  Form10.Close;
  Form7.Show;
end;

procedure TForm10.FormCreate(Sender: TObject);
begin

  FDQueryMembers.Close;
  FDQueryMembers.SQL.Text := ' select name, id, userno , email from user where userno = :userno ';
  FDQueryMembers.ParamByName('userno').AsInteger := CurrentUser.UserNo;
  FDQueryMembers.Open;

  if not FDQueryMembers.IsEmpty then
  begin
    Label6.Caption := FDQueryMembers.FieldByName('name').AsString;
    Label7.Caption := FDQueryMembers.FieldByName('id').AsString;
    Label4.Caption := FDQueryMembers.FieldByName('userno').AsString;
    Label9.Caption := FDQueryMembers.FieldByName('email').AsString;
  end
  else
  begin
    ShowMessage('DB: 불러오기 오류10');
  end;

end;

end.
