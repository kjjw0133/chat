unit ClientTestUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  REST.Types, REST.Client, Data.Bind.Components, Data.Bind.ObjectScope, System.JSON;

type
  TForm2 = class(TForm)
    PanelTop: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    EditBaseURL: TEdit;
    ComboBoxEndpoint: TComboBox;
    EditResourceld: TEdit;
    PanelMiddle: TPanel;
    Label3: TLabel;
    MemoRequestBody: TMemo;
    PanelBottom: TPanel;
    Label4: TLabel;
    MemoResponse: TMemo;
    PanelButtons: TPanel;
    BtnExecute: TButton;
    LabelStatus: TLabel;
    RESTClient1: TRESTClient;
    RESTRequest1: TRESTRequest;
    RESTResponse1: TRESTResponse;
    procedure FormCreate(Sender: TObject);
    procedure ComboBoxEndpointChange(Sender: TObject);
    procedure BtnExecuteClick(Sender: TObject);
  private
    procedure ShowExampleBody;
    procedure ExecuteRequest;
    function MethodToString(Method: TRESTRequestMethod): string;
  public
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.ComboBoxEndpointChange(Sender: TObject);
begin
  ShowExampleBody;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  // 엔드포인트 목록 초기화
  ComboBoxEndpoint.Items.Clear;
  ComboBoxEndpoint.Items.Add('POST /api/signup - 회원가입');
  ComboBoxEndpoint.Items.Add('POST /api/login - 로그인');
  ComboBoxEndpoint.Items.Add('POST /api/logout - 로그아웃');
  ComboBoxEndpoint.Items.Add('GET /api/users - 사용자 목록 조회');
  ComboBoxEndpoint.Items.Add('GET /api/users/{id} - 특정 사용자 조회');
  ComboBoxEndpoint.Items.Add('PUT /api/users/{id} - 사용자 전체 수정');
  ComboBoxEndpoint.Items.Add('DELETE /api/users/{id} - 사용자 삭제');
  ComboBoxEndpoint.Items.Add('PATCH /api/users/{id} - 사용자 수정');
  ComboBoxEndpoint.ItemIndex := 0;

  // 기본 URL 설정
  if EditBaseURL.Text = '' then
    EditBaseURL.Text := 'http://localhost:8080';

  ShowExampleBody;
end;

procedure TForm2.ShowExampleBody;
var
  JsonObj: TJSONObject;
begin
  MemoRequestBody.Lines.Clear;
  EditResourceld.Text := '';

  case ComboBoxEndpoint.ItemIndex of
    0: // POST /api/signup
    begin
      JsonObj := TJSONObject.Create;
      try
        JsonObj.AddPair('username', 'testuser');
        JsonObj.AddPair('password', '1234');
        JsonObj.AddPair('name', '홍길동');
        JsonObj.AddPair('email', 'hong@example.com');
        JsonObj.AddPair('phone', '010-1234-5678');
        MemoRequestBody.Text := JsonObj.Format;
      finally
        JsonObj.Free;
      end;
    end;

    1: // POST /api/login
    begin
      JsonObj := TJSONObject.Create;
      try
        JsonObj.AddPair('username', 'testuser');
        JsonObj.AddPair('password', '1234');
        MemoRequestBody.Text := JsonObj.Format;
      finally
        JsonObj.Free;
      end;
    end;

    2: // POST /api/logout
    begin
      JsonObj := TJSONObject.Create;
      try
        JsonObj.AddPair('username', 'testuser');
        MemoRequestBody.Text := JsonObj.Format;
      finally
        JsonObj.Free;
      end;
    end;

    3: // GET /api/users
    begin
      // Body 없음
    end;

    4: // GET /api/users/{id}
    begin
      EditResourceld.Text := '1';
    end;

    5: // PUT /api/users/{id}
    begin
      JsonObj := TJSONObject.Create;
      try
        JsonObj.AddPair('name', '홍길동(수정)');
        JsonObj.AddPair('email', 'hong_updated@example.com');
        JsonObj.AddPair('phone', '010-9999-8888');
        MemoRequestBody.Text := JsonObj.Format;
      finally
        JsonObj.Free;
      end;
      EditResourceld.Text := '1';
    end;

    6: // DELETE /api/users/{id}
    begin
      EditResourceld.Text := '1';
    end;
  end;
end;

procedure TForm2.BtnExecuteClick(Sender: TObject);
begin
  ExecuteRequest;
end;

function TForm2.MethodToString(Method: TRESTRequestMethod): string;
begin
  case Method of
    TRESTRequestMethod.rmGET: Result := 'GET';
    TRESTRequestMethod.rmPOST: Result := 'POST';
    TRESTRequestMethod.rmPUT: Result := 'PUT';
    TRESTRequestMethod.rmDELETE: Result := 'DELETE';
    TRESTRequestMethod.rmPATCH: Result := 'PATCH';
  else
    Result := 'UNKNOWN';
  end;
end;

procedure TForm2.ExecuteRequest;
var
  Method: TRESTRequestMethod;
  Endpoint: string;
  Resourceld: string;
  JsonValue: TJSONValue;
begin
  try
    BtnExecute.Enabled := False;
    LabelStatus.Caption := '요청 중...';
    Application.ProcessMessages;

    // Base URL 설정
    RESTClient1.BaseURL := EditBaseURL.Text;

    // Resource ID 가져오기
    Resourceld := Trim(EditResourceld.Text);

    // 엔드포인트와 메서드 설정
    RESTRequest1.Params.Clear;
    case ComboBoxEndpoint.ItemIndex of
      0: // POST /api/signup
      begin
        Endpoint := '/api/signup';
        Method := TRESTRequestMethod.rmPOST;
      end;

      1: // POST /api/login
      begin
        Endpoint := '/api/login';
        Method := TRESTRequestMethod.rmPOST;
      end;

      2: // POST /api/logout
      begin
        Endpoint := '/api/logout';
        Method := TRESTRequestMethod.rmPOST;
      end;

      3: // GET /api/users
      begin
        Endpoint := '/api/users';
        Method := TRESTRequestMethod.rmGET;
      end;

      4: // GET /api/users/{id}
      begin
        Endpoint := '/api/users/' + Resourceld;
        Method := TRESTRequestMethod.rmGET;
      end;

      5: // PUT /api/users/{id}
      begin
        Endpoint := '/api/users/' + Resourceld;
        Method := TRESTRequestMethod.rmPUT;
      end;

      6: // DELETE /api/users/{id}
      begin
        Endpoint := '/api/users/' + Resourceld;
        Method := TRESTRequestMethod.rmDELETE;
      end;

      7: // PATCH /api/users/[id]
      begin
        Endpoint := '/api/users/' + Resourceld;
        Method := TRESTRequestMethod.rmPATCH;
      end
    else
      Endpoint := '/api/users';
      Method := TRESTRequestMethod.rmGET;
    end;

    RESTRequest1.Resource := Endpoint;
    RESTRequest1.Method := Method;

    // Request Body 설정 (POST, PUT의 경우)
    if (Method in [TRESTRequestMethod.rmPOST, TRESTRequestMethod.rmPUT, TRESTRequestMethod.rmPATCH]) and
       (MemoRequestBody.Text <> '') then
    begin
      RESTRequest1.ClearBody;
      RESTRequest1.AddBody(MemoRequestBody.Text, TRESTContentType.ctAPPLICATION_JSON);
    end;

    // 요청 실행
    RESTRequest1.Execute;

    // 응답 표시
    MemoResponse.Lines.Clear;
    MemoResponse.Lines.Add('=== Request ===');
    MemoResponse.Lines.Add('URL: ' + RESTClient1.BaseURL + Endpoint);
    MemoResponse.Lines.Add('Method: ' + MethodToString(Method));

    if MemoRequestBody.Text <> '' then
    begin
      MemoResponse.Lines.Add('');
      MemoResponse.Lines.Add('Request Body:');
      MemoResponse.Lines.Add(MemoRequestBody.Text);
    end;

    MemoResponse.Lines.Add('');
    MemoResponse.Lines.Add('=== Response ===');
    MemoResponse.Lines.Add('Status: ' + IntToStr(RESTResponse1.StatusCode) + ' ' + RESTResponse1.StatusText);
    MemoResponse.Lines.Add('');

    // JSON 포맷팅
    try
      JsonValue := TJSONObject.ParseJSONValue(RESTResponse1.Content);
      if JsonValue <> nil then
      begin
        try
          MemoResponse.Lines.Add(JsonValue.Format);

          // 특별한 응답 메시지 표시
          if JsonValue is TJSONObject then
          begin
            var JsonObj := JsonValue as TJSONObject;
            var SuccessValue := JsonObj.GetValue('success');
            var MessageValue := JsonObj.GetValue('message');

            if (SuccessValue <> nil) and (MessageValue <> nil) then
            begin
              MemoResponse.Lines.Add('');
              MemoResponse.Lines.Add('--- 결과 ---');
              if SuccessValue is TJSONBool then
              begin
                if (SuccessValue as TJSONBool).AsBoolean then
                  MemoResponse.Lines.Add('✓ ' + MessageValue.Value)
                else
                  MemoResponse.Lines.Add('✗ ' + MessageValue.Value);
              end;
            end;
          end;
        finally
          JsonValue.Free;
        end;
      end
      else
        MemoResponse.Lines.Add(RESTResponse1.Content);
    except
      MemoResponse.Lines.Add(RESTResponse1.Content);
    end;

    // 상태 표시
    if RESTResponse1.StatusCode = 200 then
      LabelStatus.Caption := '완료! ✓ Status: ' + IntToStr(RESTResponse1.StatusCode)
    else if RESTResponse1.StatusCode = 201 then
      LabelStatus.Caption := '생성 완료! ✓ Status: ' + IntToStr(RESTResponse1.StatusCode)
    else if RESTResponse1.StatusCode >= 400 then
      LabelStatus.Caption := '오류! ✗ Status: ' + IntToStr(RESTResponse1.StatusCode)
    else
      LabelStatus.Caption := '완료! Status: ' + IntToStr(RESTResponse1.StatusCode);

  except
    on E: Exception do
    begin
      MemoResponse.Lines.Clear;
      MemoResponse.Lines.Add('=== 에러 발생 ===');
      MemoResponse.Lines.Add('에러 메시지: ' + E.Message);
      MemoResponse.Lines.Add('');
      MemoResponse.Lines.Add('서버가 실행 중인지 확인해주세요.');
      MemoResponse.Lines.Add('URL: ' + EditBaseURL.Text);
      LabelStatus.Caption := '에러 발생 ✗';
    end;
  end;

  BtnExecute.Enabled := True;
end;

end.
