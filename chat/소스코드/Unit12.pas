unit Server;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.Win.ScktComp,
  Vcl.ComCtrls;

type
  TClientInfo = class
  public
    RoomID: Integer;
    RoomName: string;
    Namespace: string;
    constructor Create;
  end;

  TForm12 = class(TForm)
    Button2: TButton;
    ServerSocket1: TServerSocket;
    RichEdit1: TRichEdit;
    procedure Button2Click(Sender: TObject);
    procedure ServerSocket1ClientRead(Sender: TObject; Socket: TCustomWinSocket);
    procedure FormCreate(Sender: TObject);
    procedure ServerSocket1ClientDisconnect(Sender: TObject; Socket: TCustomWinSocket);
  private
    Str: string;
    NowStr: string;
    username: string;
  public
  end;

var
  Form12: TForm12;

implementation

{$R *.dfm}

{ TClientInfo }

constructor TClientInfo.Create;
begin
  inherited Create;
  RoomID := -1;
  RoomName := '';
  Namespace := '/chat';
end;

procedure TForm12.Button2Click(Sender: TObject);
begin
  if not ServerSocket1.Active then
  begin
    ServerSocket1.Active := True;
    Button2.Caption := 'Stop';
  end
  else
  begin
    // 모든 연결의 Data를 해제
    try
      while ServerSocket1.Socket.ActiveConnections > 0 do
        ServerSocket1.Socket.Connections[0].Close;
    except
    end;

    ServerSocket1.Active := False;
    Button2.Caption := 'Start';
  end;
end;

procedure TForm12.FormCreate(Sender: TObject);
begin
  RichEdit1.Clear;
end;

procedure TForm12.ServerSocket1ClientDisconnect(Sender: TObject; Socket: TCustomWinSocket);
begin
  // 연결이 끊길 때 할당한 TClientInfo가 있으면 해제
  if Assigned(Socket.Data) then
  begin
    try
      TObject(Socket.Data).Free;
    except
    end;
    Socket.Data := nil;
  end;
end;

procedure TForm12.ServerSocket1ClientRead(Sender: TObject; Socket: TCustomWinSocket);
var
  i: Integer;
  RecvStr, SendStr: string;
  parts: TArray<string>;
  cmd, sRoomID, sUser, sBody: string;
  cinfo: TClientInfo;
  roomID: Integer;
  conSock: TCustomWinSocket;
begin
  NowStr := FormatDateTime('t', Now);

  RecvStr := Socket.ReceiveText;

  // 간단한 프로토콜 파싱: "JOIN::roomid::roomname::namespace" 또는 "MSG::roomid::username::body"
  if RecvStr.StartsWith('JOIN::') then
  begin
    parts := RecvStr.Split(['::']);
    // parts[0] = 'JOIN', parts[1]=roomid, parts[2]=roomname, parts[3]=namespace(optional)
    if Socket.Data = nil then
    begin
      cinfo := TClientInfo.Create;
      Socket.Data := cinfo;
    end
    else
      cinfo := TClientInfo(Socket.Data);

    if Length(parts) >= 2 then
      cinfo.RoomID := StrToIntDef(parts[1], -1);
    if Length(parts) >= 3 then
      cinfo.RoomName := parts[2];
    if Length(parts) >= 4 then
      cinfo.Namespace := parts[3]
    else
      cinfo.Namespace := '/chat';

    // 서버 로그에 표시
    RichEdit1.Paragraph.Alignment := taLeftJustify;
    RichEdit1.SelStart := RichEdit1.GetTextLen;
    RichEdit1.SelAttributes.Size := 9;
    RichEdit1.SelText := Format('Client joined room: ID=%d Name=%s NS=%s',
      [cinfo.RoomID, cinfo.RoomName, cinfo.Namespace]) + sLineBreak;
    Exit;
  end
  else if RecvStr.StartsWith('MSG::') then
  begin
    parts := RecvStr.Split(['::']);
    // parts[0]='MSG', parts[1]=roomid, parts[2]=username, parts[3]=body
    if Length(parts) >= 4 then
    begin
      sRoomID := parts[1];
      sUser := parts[2];
      // body may contain '::' inside content, join the rest
      sBody := String.Join('::', Copy(parts, 3, Length(parts) - 3));
      roomID := StrToIntDef(sRoomID, -1);

      // 서버 화면에 로그
      RichEdit1.Paragraph.Alignment := taLeftJustify;
      RichEdit1.SelStart := RichEdit1.GetTextLen;
      RichEdit1.SelAttributes.Size := 9;
      RichEdit1.SelText := sUser + sLineBreak;

      RichEdit1.SelAttributes.Size := 10;
      RichEdit1.SelText := sBody + ' : ';

      RichEdit1.SelAttributes.Size := 7;
      RichEdit1.SelText := NowStr + sLineBreak + sLineBreak;

      // 전송용 포맷 (클라이언트가 받기 쉬운 포맷)
      SendStr := Format('%s%s%s%s%s', [sUser, sLineBreak, sBody, ' : ', NowStr]);


      // 같은 roomID, 같은 namespace를 가진 연결들만 브로드캐스트
      for i := 0 to ServerSocket1.Socket.ActiveConnections - 1 do
      begin
        conSock := ServerSocket1.Socket.Connections[i];
        if conSock = Socket then
          Continue; // 보낸 본인 제외 (원하면 포함시킬 수 있음)
//          conSock.SendText(SendStr);

        if Assigned(conSock.Data) then
        begin
          try
            cinfo := TClientInfo(conSock.Data);
            if (cinfo.RoomID = roomID) then
            begin
              // (네임스페이스도 체크하려면 아래처럼 추가)
              // if cinfo.Namespace = TClientInfo(Socket.Data).Namespace then ...
              conSock.SendText(SendStr);
            end;
          except
            // ignore malformed Data
          end;
        end;
      end;
    end
    else
    begin
      // 포맷 에러 로그
      RichEdit1.Paragraph.Alignment := taLeftJustify;
      RichEdit1.SelStart := RichEdit1.GetTextLen;
      RichEdit1.SelAttributes.Size := 9;
      RichEdit1.SelText := 'Received malformed MSG: ' + RecvStr + sLineBreak;
    end;

    Exit;
  end
  else
  begin
    // 기존 동작(기본 텍스트 브로드캐스트) — 포맷 없는 텍스트는 모든 연결에 전송 (구형 호환)
    NowStr := FormatDateTime('t', Now);

    RichEdit1.Paragraph.Alignment := taLeftJustify;
    RichEdit1.SelStart := RichEdit1.GetTextLen;

    RichEdit1.SelAttributes.Size := 9;
    RichEdit1.SelText := username + sLineBreak;

    RichEdit1.SelAttributes.Size := 10;
    RichEdit1.SelText := RecvStr + ': ';

    RichEdit1.SelAttributes.Size := 7;
    RichEdit1.SelText := NowStr + sLineBreak + sLineBreak;

    SendStr :=
      username + sLineBreak +
      RecvStr + ' : ' +
      NowStr + sLineBreak + sLineBreak;

    for i := 0 to ServerSocket1.Socket.ActiveConnections - 1 do
    begin
      if ServerSocket1.Socket.Connections[i] <> Socket then
      begin
        ServerSocket1.Socket.Connections[i].SendText(SendStr);
      end;
    end;
  end;
end;

end.

