unit Unit8;

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
  TForm8 = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    FDQueryMembers: TFDQuery;
    FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink;
    FDConnection1: TFDConnection;
    LabelClose: TLabel;
    Button2: TButton;
    Button3: TButton;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure LabelCloseClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    function roomnameequal: Boolean;
  public
    ChatType : Integer;
    chatroomname : String;
  end;

var
  Form8: TForm8;

implementation

{$R *.dfm}

uses
Unit7, unit2,unit1,unit13;

function TForm8.roomnameequal: Boolean;
var
  roomname: String;
begin
  Result := True;  // 기본값 설정

  try
    // 데이터베이스 연결 확인
    if not FDConnection1.Connected then
      FDConnection1.Connected := True;

    FDQueryMembers.Close;
    FDQueryMembers.SQL.Text := 'SELECT chatroomname FROM chat WHERE chatroomname = :chatroomname';
    FDQueryMembers.ParamByName('chatroomname').AsString := chatroomname;
    FDQueryMembers.Open;

    // 레코드가 존재하는지 확인
    if not FDQueryMembers.IsEmpty then
    begin
      roomname := FDQueryMembers.FieldByName('chatroomname').AsString;
      if chatroomname = roomname then
      begin
        ShowMessage('이미 있는 이름입니다. 다시 이름으로 변경해주세요.');
        Result := False;  // 중복 발견
      end;
    end;
  except
    on E: Exception do
    begin
      ShowMessage('데이터베이스 오류: ' + E.Message);
      Result := False;
    end;
  end;
end;

procedure TForm8.Button2Click(Sender: TObject);
begin
  chatroomname := Trim(Edit1.Text);
  ChatType := 1;

  if chatroomname = '' then
  begin
    ShowMessage('방 이름을 입력해주세요.');
    Exit;
  end;

  if not roomnameequal then
    Exit;

  // Form13을 동적으로 생성
  if not Assigned(Form13) then
    Form13 := TForm13.Create(Application);

  try
    Form13.ShowModal;
  finally
    FreeAndNil(Form13);  // 사용 후 해제
  end;

  Self.Hide;
  Edit1.Clear;
end;

procedure TForm8.Button3Click(Sender: TObject);
begin
  ChatType := 2;
  chatroomname := Trim(Edit1.Text);

  if chatroomname = '' then
  begin
    ShowMessage('방 이름을 입력해주세요.');
    Exit;
  end;

  if not roomnameequal then
    Exit;

  // Form13을 동적으로 생성
  if not Assigned(Form13) then
    Form13 := TForm13.Create(Application);

  try
    Form13.ShowModal;
  finally
    FreeAndNil(Form13);  // 사용 후 해제
  end;

  Self.Hide;
  Edit1.Clear;
end;

procedure TForm8.FormCreate(Sender: TObject);
begin
  BorderStyle := bsNone;
end;

procedure TForm8.LabelCloseClick(Sender: TObject);
begin
  Close;
  Edit1.Clear;
end;

end.


