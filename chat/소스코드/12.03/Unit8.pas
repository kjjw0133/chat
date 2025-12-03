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
    Button1: TButton;
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    Edit2: TEdit;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    FDQueryMembers: TFDQuery;
    FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink;
    FDConnection1: TFDConnection;
    Label3: TLabel;
    LabelClose: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LabelCloseClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form8: TForm8;

implementation

{$R *.dfm}

uses
Unit7, unit2,unit1;

procedure TForm8.Button1Click(Sender: TObject);
type
  TLetters = array[0..25] of String;
const
  LETTERS: TLetters = (
    'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p',
    'q','r','s','t','u','v','w','x','y','z'
  );
var
  num, chatroomname, chatpw, RandChar: String;
  I, R, ChatType, ChatRoomID, userno: Integer;
begin
  chatroomname := Trim(edit1.Text);

  Randomize;
  chatpw := '';
  userno := CurrentUser.UserNo;

  if RadioButton1.Checked then
  begin
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
      chatpw := chatpw + RandChar;
    end;

    FDQueryMembers.SQL.Text :=
      'INSERT INTO chat(chatroomname, ChatType, chatpw) VALUES(:chatroomname, :ChatType, :chatpw)';
    FDQueryMembers.ParamByName('chatroomname').AsWideString := chatroomname;
    FDQueryMembers.ParamByName('chatpw').AsString := chatpw;
    FDQueryMembers.ParamByName('ChatType').AsInteger := 1;
    FDQueryMembers.ExecSQL;
    Form8.Close;

    FDQueryMembers.SQL.Text :=
      'SELECT ChatRoomID FROM chat WHERE chatroomname = :chatroomname';
    FDQueryMembers.ParamByName('chatroomname').AsWideString := chatroomname;
    FDQueryMembers.Open;

    ChatRoomID := FDQueryMembers.FieldByName('ChatRoomID').AsInteger;

    Form1.InitializeChat(ChatRoomID, 0, userno, CurrentUser.Name, chatroomname);
    ShowMessage('방 번호: ' + IntToStr(ChatRoomID) + #13#10 + '방 비밀번호: ' + chatpw);
    Form1.Show;
    Self.Hide;

    Edit1.Clear;
    Edit2.Clear;
    Exit;
  end
  else
  begin
    FDQueryMembers.SQL.Text :=
      'INSERT INTO chat(chatroomname, ChatType) VALUES(:chatroomname, :ChatType)';
    FDQueryMembers.ParamByName('chatroomname').AsWideString := chatroomname;
    FDQueryMembers.ParamByName('ChatType').AsInteger := 2;
    FDQueryMembers.ExecSQL;

    FDQueryMembers.SQL.Text :=
      'SELECT ChatRoomID FROM chat WHERE chatroomname = :chatroomname';
    FDQueryMembers.ParamByName('chatroomname').AsWideString := chatroomname;
    FDQueryMembers.Open;

    ChatRoomID := FDQueryMembers.FieldByName('ChatRoomID').AsInteger;

    Form1.InitializeChat(ChatRoomID, 0, userno, CurrentUser.Name, chatroomname);
    ShowMessage('방 번호: ' + IntToStr(ChatRoomID));

    Form1.Show;
    Self.Hide;

    Edit1.Clear;
    Edit2.Clear;
    Form8.Close;
  end;
end;

procedure TForm8.FormCreate(Sender: TObject);
begin
  BorderStyle := bsNone;
end;

procedure TForm8.LabelCloseClick(Sender: TObject);
begin
  Close;
end;

end.


