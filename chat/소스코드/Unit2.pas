unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Stan.Async, FireDAC.DApt, FireDAC.Phys.MySQLDef, FireDAC.UI.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Phys, FireDAC.Phys.MySQL,
  FireDAC.VCLUI.Wait, Data.FMTBcd, Data.SqlExpr, Data.DB, FireDAC.Comp.Client,
  FireDAC.Comp.DataSet, System.Hash; // <-- 해시 유닛 추가

type
  TForm2 = class(TForm)
    Label1: TLabel;
    Label3: TLabel;
    Edit1: TEdit; // ID
    Edit2: TEdit; // PW (비밀번호)
    Button1: TButton; // 로그인 버튼
    PanelTop: TPanel;
    LabelClose: TLabel;
    FDQueryMembers: TFDQuery;
    FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink;
    Label2: TLabel;
    Label4: TLabel;
    Button2: TButton;
    FDConnection1: TFDConnection;
    Label5: TLabel;
     procedure LabelCloseClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Label2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Label4Click(Sender: TObject);
    procedure Button2Click(Sender: TObject); // 폼 생성 이벤트 추가
  private
    { Private declarations }
  public
    function GetUserNo: Integer;
  end;

type
  TUser = record
    UserNo: Integer;
    ID: string;
    Name: string;
    Role: string; // 예: 'admin', 'user'
  end;

var
  CurrentUser: TUser;
  IsLoggedIn: Boolean = False;
  Form2: TForm2;

implementation
uses Unit3, Unit1, Unit4, Unit7;
{$R *.dfm}

procedure TForm2.FormCreate(Sender: TObject);
begin
  BorderStyle := bsNone;  // 런타임에 테두리 제거
  Edit2.PasswordChar := '*';
end;

procedure TForm2.LabelCloseClick(Sender: TObject);
begin
  Close;
end;

function TForm2.GetUserNo: Integer;
begin
  FDQueryMembers.Close;
  FDQueryMembers.SQL.Text :=
    'SELECT userno FROM user WHERE id = :userid AND pw = :userpw';
  FDQueryMembers.ParamByName('userid').AsString := Edit1.Text;
  FDQueryMembers.ParamByName('userpw').AsString := Edit2.Text;
  FDQueryMembers.Open;

  if FDQueryMembers.IsEmpty then
    Result := -1
  else
    Result := FDQueryMembers.FieldByName('userno').AsInteger;
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  //Form9.show;
end;

procedure TForm2.Button1Click(Sender: TObject);
var
  ID, PW, Salt, hashedInput: string;
begin
  ID := Trim(Edit1.Text);
  PW := Trim(Edit2.Text);

  if ID = '' then
  begin
    ShowMessage('아이디를 입력해주세요.');
    Exit;
  end;
  if PW = '' then
  begin
    ShowMessage('비밀번호를 입력해주세요.');
    Exit;
  end;

  try
    // 1. DB에서 해당 ID의 Salt 값을 먼저 가져옴
    FDQueryMembers.Close;
    FDQueryMembers.SQL.Text := 'SELECT salt FROM user WHERE id = :id';
    FDQueryMembers.ParamByName('id').AsString := ID;
    FDQueryMembers.Open;

    if FDQueryMembers.IsEmpty then
    begin
      ShowMessage('아이디 또는 비밀번호가 잘못되었습니다.');
      Exit;
    end;

    // 2. 가져온 Salt로 입력한 비밀번호를 해시
    Salt := FDQueryMembers.FieldByName('salt').AsString;
    hashedInput := THashSHA2.GetHashString(PW + Salt);

    // 3. 해시된 비밀번호로 로그인 시도
    FDQueryMembers.Close;
    FDQueryMembers.SQL.Text :=
      'SELECT userno, id, name, role FROM user WHERE id = :id AND pw = :pw';
    FDQueryMembers.ParamByName('id').AsString := ID;
    FDQueryMembers.ParamByName('pw').AsString := hashedInput;
    FDQueryMembers.Open;

    if not FDQueryMembers.IsEmpty then
    begin
      CurrentUser.ID := FDQueryMembers.FieldByName('id').AsString;
      CurrentUser.Name := FDQueryMembers.FieldByName('name').AsString;
      CurrentUser.Role := FDQueryMembers.FieldByName('role').AsString;
      CurrentUser.UserNo := FDQueryMembers.FieldByName('userno').AsInteger;
      IsLoggedIn := True;

      // 중복 로그인 체크
      FDQueryMembers.Close;
      FDQueryMembers.SQL.Text := 'SELECT is_logged_in FROM user WHERE userno = :userno';
      FDQueryMembers.ParamByName('userno').AsInteger := CurrentUser.UserNo;
      FDQueryMembers.Open;

      if FDQueryMembers.FieldByName('is_logged_in').AsBoolean then
      begin
        ShowMessage('이미 로그인한 계정입니다.');
        Exit;
      end
      else
      begin
        FDQueryMembers.Close;
        FDQueryMembers.SQL.Text := 'UPDATE user SET is_logged_in = 1 WHERE userno = :userno';
        FDQueryMembers.ParamByName('userno').AsInteger := CurrentUser.UserNo;
        FDQueryMembers.ExecSQL;
      end;

      Form7.UserNo := CurrentUser.UserNo;
      Edit1.Clear;
      Edit2.Clear;
      ModalResult := mrOk;
    end
    else
      ShowMessage('아이디 또는 비밀번호가 잘못되었습니다.');
  finally
    FDQueryMembers.Close;
  end;
end;

procedure TForm2.Label2Click(Sender: TObject);
begin
  Form3.Show;
  Form2.Close;
end;

procedure TForm2.Label4Click(Sender: TObject);
begin
  Form4.Show;
  Form2.Close;
end;

end.

