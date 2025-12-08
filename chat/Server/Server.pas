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
    ServerSocket1: TServerSocket;
    RichEdit1: TRichEdit;
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

procedure TForm12.FormCreate(Sender: TObject);
begin
  if not ServerSocket1.Active then
  begin
    ServerSocket1.Active := True;
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
  end;

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
  sRoomID, sUser, sBody: string;
  cinfo: TClientInfo;
  roomID: Integer;
  conSock: TCustomWinSocket;
begin
  NowStr := FormatDateTime('t', Now);
  RecvStr := Socket.ReceiveText;

  if RecvStr.StartsWith('JOIN::') then
  begin
    parts := RecvStr.Split(['::']);
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

    RichEdit1.Paragraph.Alignment := taLeftJustify;
    RichEdit1.SelStart := RichEdit1.GetTextLen;
    RichEdit1.SelAttributes.Size := 9;
    RichEdit1.SelText := Format('Client joined room: ID=%d Name=%s NS=%s',
      [cinfo.RoomID, cinfo.RoomName, cinfo.Namespace]) + sLineBreak;
    Exit;
  end

  else if RecvStr.StartsWith('JOIN_MSG::') then
  begin
    parts := RecvStr.Split(['::']);
    if Length(parts) >= 3 then
    begin
      sRoomID := parts[1];
      sUser := parts[2];
      roomID := StrToIntDef(sRoomID, -1);

      // 서버 로그
      RichEdit1.Paragraph.Alignment := taLeftJustify;
      RichEdit1.SelStart := RichEdit1.GetTextLen;
      RichEdit1.SelAttributes.Size := 9;
      RichEdit1.SelAttributes.Color := clGreen;
      RichEdit1.SelText := Format('[입장] %s님이 방 %s에 입장했습니다. [%s]',
        [sUser, sRoomID, NowStr]) + sLineBreak;

      // 같은 방의 모든 클라이언트에게 메시지 (본인 제외)
      SendStr := Format('JOIN_MSG::%s::%s', [sRoomID, sUser]);

      for i := 0 to ServerSocket1.Socket.ActiveConnections - 1 do
      begin
        conSock := ServerSocket1.Socket.Connections[i];
        if conSock = Socket then
          Continue; // 본인 제외

        if Assigned(conSock.Data) then
        begin
          try
            cinfo := TClientInfo(conSock.Data);
            if (cinfo.RoomID = roomID) then
            begin
              conSock.SendText(SendStr);
            end;
          except
          end;
        end;
      end;
    end;
    Exit;
  end

  else if RecvStr.StartsWith('LEAVE::') then // 퇴장 메시지 
  begin
    parts := RecvStr.Split(['::']);
    if Length(parts) >= 3 then
    begin
      sRoomID := parts[1];
      sUser := parts[2];
      roomID := StrToIntDef(sRoomID, -1);
      
      RichEdit1.Paragraph.Alignment := taLeftJustify;
      RichEdit1.SelStart := RichEdit1.GetTextLen;
      RichEdit1.SelAttributes.Size := 9;
      RichEdit1.SelAttributes.Color := clRed;
      RichEdit1.SelText := Format('[퇴장] %s님이 방 %s에서 퇴장했습니다. [%s]',
        [sUser, sRoomID, NowStr]) + sLineBreak;

      SendStr := Format('LEAVE::%s::%s', [sRoomID, sUser]);

      for i := 0 to ServerSocket1.Socket.ActiveConnections - 1 do
      begin
        conSock := ServerSocket1.Socket.Connections[i];

        if Assigned(conSock.Data) then
        begin
          try
            cinfo := TClientInfo(conSock.Data);
            if (cinfo.RoomID = roomID) then
            begin
              conSock.SendText(SendStr);
            end;
          except
          end;
        end;
      end;
    end;
    Exit;
  end

  else if RecvStr.StartsWith('MSG::') then
  begin
    parts := RecvStr.Split(['::']);
    if Length(parts) >= 4 then
    begin
      sRoomID := parts[1];
      sUser := parts[2];
      sBody := String.Join('::', Copy(parts, 3, Length(parts) - 3));
      roomID := StrToIntDef(sRoomID, -1);

      RichEdit1.Paragraph.Alignment := taLeftJustify;
      RichEdit1.SelStart := RichEdit1.GetTextLen;
      RichEdit1.SelAttributes.Size := 9;
      RichEdit1.SelAttributes.Color := clBlack;
      RichEdit1.SelText := sUser + ': ';
      RichEdit1.SelAttributes.Size := 10;
      RichEdit1.SelText := sBody + ' [' + NowStr + ']' + sLineBreak;

      SendStr := Format('MSG::%s::%s::%s', [sRoomID, sUser, sBody]);

      for i := 0 to ServerSocket1.Socket.ActiveConnections - 1 do
      begin
        conSock := ServerSocket1.Socket.Connections[i];
        if conSock = Socket then
          Continue; 

        if Assigned(conSock.Data) then
        begin
          try
            cinfo := TClientInfo(conSock.Data);
            if (cinfo.RoomID = roomID) then
            begin
              conSock.SendText(SendStr);
            end;
          except
          end;
        end;
      end;
    end;
    Exit;
  end;
end;

end.





