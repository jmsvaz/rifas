{
This file is part of Rifas - a raffle generator program.
Copyright (C) 2011 João Marcelo S. Vaz

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
}

unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, StdCtrls, ExtCtrls, EditBtn, Spin, ExtDlgs,
  Graphics, LR_Class, LR_DSet, lr_e_pdf, DefaultTranslator;

type

  { TMainForm }

TMainForm = class(TForm)
    btCreate: TButton;
    btAbout: TButton;
    btClose: TButton;
    edAward: TLabeledEdit;
    edDate: TDateEdit;
    edImageFile: TEditButton;
    edPlace: TLabeledEdit;
    edPrice: TFloatSpinEdit;
    frReport: TfrReport;
    frPDFExport: TfrTNPDFExport;
    frUserDataset: TfrUserDataset;
    gbRaffle: TGroupBox;
    gbMain: TGroupBox;
    gbAward: TGroupBox;
    gbTickets: TGroupBox;
    gbNames: TGroupBox;
    edTitle: TLabeledEdit;
    imAward: TImage;
    lbLastNumber: TLabel;
    lbFirstNumber: TLabel;
    lbPrice: TLabel;
    lbAward: TBoundLabel;
    lbDate: TLabel;
    lbFirsNumber: TBoundLabel;
    lbImage: TLabel;
    lbPlace: TBoundLabel;
    lbTitle: TBoundLabel;
    lbValue: TBoundLabel;
    mmPeople: TMemo;
    edLastNumber: TSpinEdit;
    edFirstNumber: TSpinEdit;
    OpenPictureDialog: TOpenPictureDialog;
    pnImage: TPanel;
    procedure btAboutClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure btCreateClick(Sender: TObject);
    procedure edImageFileButtonClick(Sender: TObject);
    procedure edImageFileChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure frReportBeginDoc;
    procedure frReportGetValue(const ParName: String; var ParValue: Variant);
    procedure frUserDatasetCheckEOF(Sender: TObject; var Eof: Boolean);
    procedure frUserDatasetFirst(Sender: TObject);
    procedure frUserDatasetNext(Sender: TObject);
    procedure imAwardDblClick(Sender: TObject);
    procedure OpenPictureDialogClose(Sender: TObject);
  private
    AwardImage: TBitmap;
    procedure ClearImage;
    procedure TestIfAwardIsValid;
    procedure TestIfTicketNumbersAreValid;
    procedure TestIfTicketsQuantityIsMultiple;
    procedure LoadImageFile(FileName: string);
    function ResampleBitmap(NewHeight, NewWidth: Integer): TBitmap;
    { private declarations }
  public
    { public declarations }
    Count: Integer;
    procedure Generate;
  end; 

var
  MainForm: TMainForm;

implementation

uses Math, uStrings, uAbout;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormShow(Sender: TObject);
begin
  Caption:= Application.Title;
end;

procedure TMainForm.TestIfAwardIsValid;
begin
  if Length(edAward.Text) <= 0 then
    raise Exception.Create(sAwardCantBeBlank);
end;

procedure TMainForm.TestIfTicketNumbersAreValid;
begin
  if edLastNumber.Value < edFirstNumber.Value then
    raise Exception.Create(sTicketNumbersInvalid);
end;

procedure TMainForm.TestIfTicketsQuantityIsMultiple;
begin
  if ((edLastNumber.Value - edFirstNumber.Value + 1) mod mmPeople.Lines.Count) > 0 then
    raise Exception.Create(sTicketsQuantityNotMultiple);
end;

procedure TMainForm.imAwardDblClick(Sender: TObject);
begin
  OpenPictureDialog.Execute;
end;

procedure TMainForm.edImageFileButtonClick(Sender: TObject);
begin
  OpenPictureDialog.Execute;
end;

procedure TMainForm.OpenPictureDialogClose(Sender: TObject);
begin
  edImageFile.Text:= OpenPictureDialog.FileName;
end;


procedure TMainForm.edImageFileChange(Sender: TObject);
begin
  if FileExists(edImageFile.Text) then
    LoadImageFile(edImageFile.Text)
  else
    ClearImage;
end;

procedure TMainForm.LoadImageFile(FileName: string);
var
  Picture: TPicture;
begin
  Picture := TPicture.Create;
  try
    Picture.LoadFromFile(Filename);
    AwardImage.Assign(Picture.Bitmap);
  finally
    Picture.Free
  end;
  imAward.Picture.Bitmap:= ResampleBitmap(imAward.Height,imAward.Width);
end;

procedure TMainForm.ClearImage;
begin
  AwardImage.Clear;
  imAward.Picture.Clear;
end;

function TMainForm.ResampleBitmap(NewHeight, NewWidth: Integer): TBitmap;
var
  ARect: TRect;
begin
  Result:= TBitmap.Create;
  try
    Result.Assign(AwardImage);
    if ((Result.Height > NewHeight) or (Result.Width > NewWidth)) then
      begin
        ARect.Left:= 0;
        ARect.Top:= 0;
        ARect.Right:= Result.Width;
        ARect.Bottom:= Result.Height;
        if Result.Width > NewWidth then
          begin
            ARect.Right := NewWidth;
            ARect.Bottom := (ARect.Right * Result.Height) div Result.Width;
          end;
        if Result.Height > NewHeight then
          begin
            ARect.Bottom := Min(NewHeight,ARect.Bottom);
            ARect.Right := (ARect.Bottom * Result.Width) div Result.Height;
          end;
          Result.Canvas.StretchDraw(ARect, Result) ;
          Result.Width := ARect.Right;
          Result.Height := ARect.Bottom;
      end;
  finally
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  AwardImage:= TBitmap.Create;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  AwardImage.Free;
end;

procedure TMainForm.btCreateClick(Sender: TObject);
begin
  try
    TestIfAwardIsValid;
    TestIfTicketNumbersAreValid;
    if mmPeople.Lines.Count > 0 then
      TestIfTicketsQuantityIsMultiple;
    Generate;
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

procedure TMainForm.btCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.btAboutClick(Sender: TObject);
begin
  uAbout.AboutDialog.ShowModal;
end;

procedure TMainForm.Generate;
begin
  if frReport.PrepareReport then
    begin
      frReport.Title:= edTitle.Text;
      frReport.ShowPreparedReport;
    end;
end;

procedure TMainForm.frReportGetValue(const ParName: String; var ParValue: Variant);
begin
  if ParName = 'Name' then
    ParValue:= Format('%s:', [sNameCaption]);
  if ParName = 'Phone' then
    ParValue:= Format('%s:', [sPhoneCaption]);
  if ParName = 'Title' then
    ParValue:= edTitle.Text;
  if ParName = 'Award' then
    ParValue:= edAward.Text;
  if ParName = 'Date' then
    ParValue:= Format('%s: %s', [sDateCaption,edDate.Text]);
  if ParName = 'Place' then
    ParValue:= Format('%s: %s', [sPlaceCaption,edPlace.Text]);
  if ParName = 'Number' then
    ParValue:= Format('%.*d', [Length(edLastNumber.Text), Count]) ;
  if ParName = 'Price' then
    ParValue:= Format('%m', [edPrice.Value]);
  if mmPeople.Lines.Count > 0 then
    if ParName = 'Person' then
      ParValue:= mmPeople.Lines[(Count - edFirstNumber.Value) div ((edLastNumber.Value - edFirstNumber.Value + 1) div mmPeople.Lines.Count)];
end;

procedure TMainForm.frReportBeginDoc;
var
  AHeight, AWidth: Integer;
  pic: TfrObject;
begin
  pic:= frReport.FindObject('lrPicture');
  if (pic is TfrPictureView) then
    begin
      (pic as TfrPictureView).Stretched:= False;
      AHeight:= Trunc((pic as TfrPictureView).Height);
      AWidth:=  Trunc((pic as TfrPictureView).Width);
      (pic as TfrPictureView).Picture.Bitmap.Assign(ResampleBitmap(AHeight,AWidth));
    end;
end;

procedure TMainForm.frUserDatasetCheckEOF(Sender: TObject; var Eof: Boolean);
begin
  Eof:= Count > edLastNumber.Value;
end;

procedure TMainForm.frUserDatasetFirst(Sender: TObject);
begin
  Count:= edFirstNumber.Value;
end;

procedure TMainForm.frUserDatasetNext(Sender: TObject);
begin
  Inc(Count);
end;

end.

