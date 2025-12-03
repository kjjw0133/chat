unit Unit13;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.ComCtrls, math;

type
  TForm13 = class(TForm)
    LabelTitle: TLabel;
    pnlSearch: TPanel;
    pbSearchBG: TPaintBox;
    edtSearch: TEdit;
    lbFriends: TListBox;
    PanelBottom: TPanel;
    btnOK: TButton;
    btnCancel: TButton;

    procedure FormCreate(Sender: TObject);
    procedure pbSearchBGPaint(Sender: TObject);
    procedure edtSearchChange(Sender: TObject);
    procedure edtSearchEnter(Sender: TObject);
    procedure edtSearchExit(Sender: TObject);
    procedure lbFriendsMeasureItem(Control: TWinControl; Index: Integer;
      var Height: Integer);
    procedure lbFriendsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure lbFriendsClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    FAllNames: TStringList;
    procedure PopulateAllNames;
    procedure FilterList(const Keyword: string);
    procedure DrawMagnifier(const C: TCanvas; const R: TRect);
  public
  end;

var
  Form13: TForm13;

implementation

{$R *.dfm}

const
  AVATAR_SIZE = 36;    // 프로필 아이콘 크기
  ITEM_PADDING = 8;
  RADIUS = 24;         // 검색창 — 카톡 스타일 완전 둥근 곡률

procedure TForm13.FormCreate(Sender: TObject);
var
  Rgn: HRGN;
begin
  FAllNames := TStringList.Create;
  PopulateAllNames;

  lbFriends.Style := lbOwnerDrawVariable;
  lbFriends.DoubleBuffered := True;

  FilterList('');

  pbSearchBG.SendToBack;

  // 패널을 둥글게 만들기
  Rgn := CreateRoundRectRgn(0, 0, pnlSearch.Width + 1, pnlSearch.Height + 1,
                            pnlSearch.Height, pnlSearch.Height);
  SetWindowRgn(pnlSearch.Handle, Rgn, True);
end;

{ ---------------- Data load ---------------- }
procedure TForm13.PopulateAllNames;
begin
  FAllNames.Clear;
  FAllNames.Add('test1');
  FAllNames.Add('test2');
  FAllNames.Add('test3');
  FAllNames.Add('test4');
  FAllNames.Add('test5');
  FAllNames.Add('test6');
  FAllNames.Add('test7');
  FAllNames.Add('test8');
  FAllNames.Add('test9');
  FAllNames.Add('test10');
end;

{ ---------------- Filter ---------------- }
procedure TForm13.FilterList(const Keyword: string);
var
  i: Integer;
  s: string;
begin
  lbFriends.Items.BeginUpdate;
  try
    lbFriends.Items.Clear;

    for i := 0 to FAllNames.Count - 1 do
    begin
      s := FAllNames[i];
      if (Keyword = '') or (Pos(LowerCase(Keyword), LowerCase(s)) > 0) then
        lbFriends.Items.Add(s);
    end;

    lbFriends.ItemIndex := -1;
  finally
    lbFriends.Items.EndUpdate;
  end;

  lbFriends.Invalidate;
end;

procedure TForm13.edtSearchChange(Sender: TObject);
begin
  FilterList(Trim(edtSearch.Text));
end;

procedure TForm13.edtSearchEnter(Sender: TObject);
begin
  pbSearchBG.Invalidate;
end;

procedure TForm13.edtSearchExit(Sender: TObject);
begin
  pbSearchBG.Invalidate;
end;

{ ---------------- PaintBox: search bar ---------------- }
procedure TForm13.pbSearchBGPaint(Sender: TObject);
var
  C: TCanvas;
  R: TRect;
  PenCol: TColor;
begin
  C := pbSearchBG.Canvas;
  R := pbSearchBG.ClientRect;

  if edtSearch.Focused then
    PenCol := RGB(100,160,255)
  else
    PenCol := $00C8C8C8;

  C.Brush.Style := bsSolid;
  C.Brush.Color := clWhite;  // 배경을 흰색으로
  C.Pen.Style := psSolid;
  C.Pen.Color := PenCol;
  C.Pen.Width := 1;

  InflateRect(R, -1, -1);

  C.RoundRect(
    R.Left, R.Top, R.Right, R.Bottom,
    R.Height, R.Height
  );

  DrawMagnifier(C, Rect(
    R.Left + 8,
    R.Top + (R.Height - 20) div 2,
    R.Left + 8 + 20,
    R.Top + (R.Height - 20) div 2 + 20
  ));
end;

{ ---------------- Magnifier icon ---------------- }
procedure TForm13.DrawMagnifier(const C: TCanvas; const R: TRect);
//var
//  cx, cy, r: Integer;
//  OldPenWidth: Integer;
begin
//  cx := (R.Left + R.Right) div 2 - 2;
//  cy := (R.Top + R.Bottom) div 2 - 2;
//  r  := 6;
//  OldPenWidth := C.Pen.Width;
//  C.Pen.Width := 1;
//  C.Pen.Color := clGray;
//  C.Brush.Style := bsClear;
//
//  C.Ellipse(cx - r, cy - r, cx + r, cy + r);
//
//  C.MoveTo(cx + Round(r*0.7), cy + Round(r*0.7));
//  C.LineTo(cx + r + 6, cy + r + 6);
//
//  C.Pen.Width := OldPenWidth;
end;

{ ---------------- List item height ---------------- }
procedure TForm13.lbFriendsMeasureItem(Control: TWinControl; Index: Integer;
  var Height: Integer);
begin
  Height := Max(AVATAR_SIZE + ITEM_PADDING, 46);
end;

{ ---------------- Owner Draw list ---------------- }
procedure TForm13.lbFriendsDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  C: TCanvas;
  sName: string;
  isSel: Boolean;
  YCenter: Integer;
  avatarRect, radioRect, R: TRect;
  colBg: TColor;
  TextLeft: Integer;
begin
  C := lbFriends.Canvas;
  sName := lbFriends.Items[Index];
  isSel := (lbFriends.ItemIndex = Index);

  if odSelected in State then
    C.Brush.Color := $00F0F8FF
  else
    C.Brush.Color := clWhite;
  C.FillRect(Rect);

  YCenter := (Rect.Top + Rect.Bottom) div 2;

  avatarRect.Left   := Rect.Left + ITEM_PADDING;
  avatarRect.Right  := avatarRect.Left + AVATAR_SIZE;
  avatarRect.Top    := YCenter - AVATAR_SIZE div 2;
  avatarRect.Bottom := avatarRect.Top + AVATAR_SIZE;

  colBg := RGB(
    150 + (Ord(sName[1]) * 3) mod 100,
    120 + (Ord(sName[1]) * 5) mod 100,
    160 + (Ord(sName[1]) * 7) mod 80
  );

  C.Brush.Color := colBg;
  C.Pen.Color := colBg;
  C.Ellipse(avatarRect.Left, avatarRect.Top, avatarRect.Right, avatarRect.Bottom);

  C.Font.Name := 'Segoe UI';
  C.Font.Size := 10;
  C.Font.Style := [fsBold];
  C.Font.Color := clWhite;

  DrawText(C.Handle, PChar(Copy(sName,1,1)), 1,
    avatarRect, DT_CENTER or DT_VCENTER or DT_SINGLELINE);

  TextLeft := avatarRect.Right + ITEM_PADDING;

  R := Rect;
  R.Left := TextLeft;
  R.Right := Rect.Right - 40;

  C.Font.Size := 11;
  C.Font.Style := [];
  C.Font.Color := clWindowText;

  DrawText(C.Handle, PChar(sName), Length(sName), R,
    DT_LEFT or DT_VCENTER or DT_SINGLELINE);

  radioRect.Left := Rect.Right - ITEM_PADDING - 18;
  radioRect.Right := radioRect.Left + 18;
  radioRect.Top := YCenter - 9;
  radioRect.Bottom := radioRect.Top + 18;

  C.Brush.Style := bsClear;
  C.Pen.Color := $00C8C8C8;
  C.Ellipse(radioRect.Left, radioRect.Top, radioRect.Right, radioRect.Bottom);

  if isSel then
  begin
    C.Brush.Style := bsSolid;
    C.Brush.Color := RGB(100,160,255);
    C.Pen.Color := C.Brush.Color;
    C.Ellipse(radioRect.Left+5, radioRect.Top+5,
              radioRect.Right-5, radioRect.Bottom-5);
  end;
end;

procedure TForm13.lbFriendsClick(Sender: TObject);
begin
  lbFriends.Invalidate;
end;

procedure TForm13.btnOKClick(Sender: TObject);
begin
  if lbFriends.ItemIndex >= 0 then
    ShowMessage('선택: ' + lbFriends.Items[lbFriends.ItemIndex])
  else
    ShowMessage('항목을 선택하세요.');
end;

end.

