unit FriendManager;

interface

uses
  System.SysUtils, System.Generics.Collections, FireDAC.Comp.Client, Data.DB;

type
  TFriendInfo = class
    UserId: String;
    UserNo: Integer;
    UserName: WideString;
    IsSelected: Boolean;
  end;

  TFriendManager = class
  private
    FFriendList: TObjectList<TFriendInfo>;
    FConnection: TFDConnection;
  public
    constructor Create(AConnection: TFDConnection);
    destructor Destroy; override;

    function LoadFriends(CurrentUserNo: Integer): Boolean;
    function GetFriendList: TObjectList<TFriendInfo>;
    function GetFriendCount: Integer;
    function FilterByKeyword(const Keyword: string): TList<TFriendInfo>;
    procedure ClearSelection;
  end;

implementation

{ TFriendManager }

constructor TFriendManager.Create(AConnection: TFDConnection);
begin
  inherited Create;
  FFriendList := TObjectList<TFriendInfo>.Create(True);
  FConnection := AConnection;
end;

destructor TFriendManager.Destroy;
begin
  FFriendList.Free;
  inherited;
end;

function TFriendManager.LoadFriends(CurrentUserNo: Integer): Boolean;
var
  Query: TFDQuery;
  FriendInfo: TFriendInfo;
  CurrentUserId: String;
begin
  Result := False;
  FFriendList.Clear;

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;

    // 1. UserNo로 UserId 조회
    Query.SQL.Text := 'SELECT id FROM user WHERE userno = :userno';
    Query.ParamByName('userno').AsInteger := CurrentUserNo;
    Query.Open;

    if Query.RecordCount = 0 then
      Exit;

    CurrentUserId := Query.FieldByName('id').AsString;
    Query.Close;

    // 2. 친구 목록 조회
    Query.SQL.Text :=
      'SELECT ' +
      '  CASE ' +
      '    WHEN f.requester_id = :current_user_id THEN u2.userno ' +
      '    ELSE u1.userno ' +
      '  END AS friend_userno, ' +
      '  CASE ' +
      '    WHEN f.requester_id = :current_user_id THEN f.receiver_id ' +
      '    ELSE f.requester_id ' +
      '  END AS friend_id, ' +
      '  CASE ' +
      '    WHEN f.requester_id = :current_user_id THEN u2.name ' +
      '    ELSE u1.name ' +
      '  END AS friend_name ' +
      'FROM friend f ' +
      'LEFT JOIN user u1 ON f.requester_id = u1.id ' +
      'LEFT JOIN user u2 ON f.receiver_id = u2.id ' +
      'WHERE (f.requester_id = :current_user_id OR f.receiver_id = :current_user_id) ' +
      '  AND f.status = 2 ' +
      'ORDER BY friend_name';

    Query.ParamByName('current_user_id').AsString := CurrentUserId;
    Query.Open;

    while not Query.Eof do
    begin
      FriendInfo := TFriendInfo.Create;
      FriendInfo.UserNo := Query.FieldByName('friend_userno').AsInteger;
      FriendInfo.UserId := Query.FieldByName('friend_id').AsString;
      FriendInfo.UserName := Query.FieldByName('friend_name').AsString;
      FriendInfo.IsSelected := False;

      FFriendList.Add(FriendInfo);
      Query.Next;
    end;

    Result := True;
  finally
    Query.Free;
  end;
end;

function TFriendManager.GetFriendList: TObjectList<TFriendInfo>;
begin
  Result := FFriendList;
end;

function TFriendManager.GetFriendCount: Integer;
begin
  Result := FFriendList.Count;
end;

function TFriendManager.FilterByKeyword(const Keyword: string): TList<TFriendInfo>;
var
  i: Integer;
  Friend: TFriendInfo;
  LowerKeyword: string;
begin
  Result := TList<TFriendInfo>.Create;
  LowerKeyword := LowerCase(Trim(Keyword));

  for i := 0 to FFriendList.Count - 1 do
  begin
    Friend := FFriendList[i];
    if (LowerKeyword = '') or
       (Pos(LowerKeyword, LowerCase(Friend.UserName)) > 0) then
    begin
      Result.Add(Friend);
    end;
  end;
end;

procedure TFriendManager.ClearSelection;
var
  i: Integer;
begin
  for i := 0 to FFriendList.Count - 1 do
    FFriendList[i].IsSelected := False;
end;

end.
