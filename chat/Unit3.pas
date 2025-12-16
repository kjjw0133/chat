unit Unit3;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, FireDAC.Stan.Async, FireDAC.DApt,
  FireDAC.Phys.MySQLDef, FireDAC.UI.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool,
  FireDAC.Phys, FireDAC.Phys.MySQL, FireDAC.VCLUI.Wait, Data.DB,
  FireDAC.Comp.Client, FireDAC.Comp.DataSet, Data.SqlExpr, Data.FMTBcd, System.Hash;

type
  TForm3 = class(TForm)
    Label1: TLabel;
    Label3: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Button1: TButton;
    Label2: TLabel;
    Edit3: TEdit;
    FDQueryMembers: TFDQuery;
    Label4: TLabel;
    Edit4: TEdit;
    ComboBox1: TComboBox;
    FDConnection1: TFDConnection;
    Label5: TLabel;
    LabelClose: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LabelCloseClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

uses Unit2, Unit7;

{$R *.dfm}

procedure TForm3.Button1Click(Sender: TObject);
var
  ID, PW, Name, Email, Salt, hashedInput: string;
  NewUserNo: Integer;
begin
  ID := Trim(Edit1.Text);
  PW := Trim(Edit2.Text);
  Name := Trim(Edit3.Text);
  Email := Trim(Edit4.Text) + ComboBox1.Text;

  if ID.IsEmpty or PW.IsEmpty or Name.IsEmpty or Email.IsEmpty then
  begin
    ShowMessage('모든 항목을 입력해주세요.');
    Exit;
  end;

  // 1. 랜덤 Salt 생성 (GUID 사용)
  Salt := THashSHA2.GetHashString(TGUID.NewGuid.ToString + DateTimeToStr(Now));

  hashedInput := THashSHA2.GetHashString(PW + Salt);

  try
    FDQueryMembers.SQL.Text :=
      'INSERT INTO user(id, pw, salt, name, email) VALUES (:id, :pw, :salt, :name, :email)';
    FDQueryMembers.ParamByName('id').AsString   := ID;
    FDQueryMembers.ParamByName('pw').AsString   := hashedInput;
    FDQueryMembers.ParamByName('salt').AsString := Salt;
    FDQueryMembers.ParamByName('name').AsString := Name;
    FDQueryMembers.ParamByName('email').AsString:= Email;
    FDQueryMembers.ExecSQL;

    FDQueryMembers.Close;
    FDQueryMembers.SQL.Text :=
      'SELECT userno, id, name, role, is_logged_in FROM user WHERE id = :id';
    FDQueryMembers.ParamByName('id').AsString := ID;
    FDQueryMembers.Open;

    if not FDQueryMembers.IsEmpty then
    begin
      CurrentUser.UserNo := FDQueryMembers.FieldByName('userno').AsInteger;
      CurrentUser.ID := FDQueryMembers.FieldByName('id').AsString;
      CurrentUser.Name := FDQueryMembers.FieldByName('name').AsString;
      CurrentUser.Role := FDQueryMembers.FieldByName('role').AsString;
      IsLoggedIn := True;

      if not FDQueryMembers.FieldByName('is_logged_in').AsBoolean then
      begin
        FDQueryMembers.Close;
        FDQueryMembers.SQL.Text :=
          'UPDATE user SET is_logged_in = 1 WHERE userno = :userno';
        FDQueryMembers.ParamByName('userno').AsInteger := CurrentUser.UserNo;
        FDQueryMembers.ExecSQL;
      end;

      ShowMessage('회원가입 성공: ' + CurrentUser.Name);
      Form7.UserNo := CurrentUser.UserNo;

      Edit1.Clear;
      Edit2.Clear;
      Edit3.Clear;
      Edit4.Clear;
      ComboBox1.Clear;
      ModalResult := mrOk;
    end
    else
      ShowMessage('회원가입은 되었지만 로그인 정보를 가져오지 못했습니다.');
  finally
    FDQueryMembers.Close;
  end;
end;

procedure TForm3.ComboBox1Change(Sender: TObject);
begin
  ComboBox1.Style := csDropDown;
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
   BorderStyle := bsNone;
end;

procedure TForm3.LabelCloseClick(Sender: TObject);
begin
  close;
end;

end.

