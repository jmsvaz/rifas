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
  Classes, SysUtils, Forms, Dialogs, StdCtrls, ExtCtrls, EditBtn, Spin,
  LR_Class, LR_DSet, lr_e_pdf, DefaultTranslator, ExtDlgs;

type

  { TfmMain }

TfmMain = class(TForm)
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
    gbPeople: TGroupBox;
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
    procedure FormShow(Sender: TObject);
    procedure frReportEnterRect(Memo: TStringList; View: TfrView);
    procedure frReportGetValue(const ParName: String; var ParValue: Variant);
    procedure frUserDatasetCheckEOF(Sender: TObject; var Eof: Boolean);
    procedure frUserDatasetFirst(Sender: TObject);
    procedure frUserDatasetNext(Sender: TObject);
    procedure imAwardDblClick(Sender: TObject);
    procedure OpenPictureDialogClose(Sender: TObject);
  private
    procedure TestIfAwardIsValid;
    procedure TestIfTicketNumbersAreValid;
    procedure TestIfTicketsQuantityIsMultiple;
    { private declarations }
  public
    { public declarations }
    Count: Integer;
    procedure Generate;
  end; 

var
  fmMain: TfmMain;

implementation

uses uStrings, uAbout;

{$R *.lfm}

{ TfmMain }


procedure TfmMain.FormShow(Sender: TObject);
begin
  Caption:= Application.Title;
end;


procedure TfmMain.TestIfAwardIsValid;
begin
  if Length(edAward.Text) <= 0 then
    raise Exception.Create(sAwardCantBeBlank);
end;

procedure TfmMain.TestIfTicketNumbersAreValid;
begin
  if edLastNumber.Value < edFirstNumber.Value then
    raise Exception.Create(sTicketNumbersInvalid);
end;

procedure TfmMain.TestIfTicketsQuantityIsMultiple;
begin
  if ((edLastNumber.Value - edFirstNumber.Value + 1) mod mmPeople.Lines.Count) > 0 then
    raise Exception.Create(sTicketsQuantityNotMultiple);
end;

procedure TfmMain.btCreateClick(Sender: TObject);
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

procedure TfmMain.edImageFileButtonClick(Sender: TObject);
begin
  OpenPictureDialog.Execute;
end;

procedure TfmMain.edImageFileChange(Sender: TObject);
begin
  if FileExists(edImageFile.Text) then
    imAward.Picture.LoadFromFile(edImageFile.Text)
  else
    imAward.Picture.Clear;
end;

procedure TfmMain.imAwardDblClick(Sender: TObject);
begin
  OpenPictureDialog.Execute;
end;

procedure TfmMain.OpenPictureDialogClose(Sender: TObject);
begin
  edImageFile.Text:= OpenPictureDialog.FileName;
end;

procedure TfmMain.btCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfmMain.btAboutClick(Sender: TObject);
begin
  uAbout.fmAbout.ShowModal;
end;

procedure TfmMain.Generate;
begin
  if frReport.PrepareReport then
    begin
      frReport.Title:= edTitle.Text;
      frReport.ShowPreparedReport;
    end;
end;

procedure TfmMain.frReportGetValue(const ParName: String; var ParValue: Variant);
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
    //ParValue:= Format('%s: %m', [sPriceCaption, edPrice.Value]);
    ParValue:= Format('%m', [edPrice.Value]);
  if mmPeople.Lines.Count > 0 then
    if ParName = 'Person' then
      ParValue:= mmPeople.Lines[(Count - edFirstNumber.Value) div ((edLastNumber.Value - edFirstNumber.Value + 1) div mmPeople.Lines.Count)];
end;

procedure TfmMain.frReportEnterRect(Memo: TStringList; View: TfrView);
begin
  if Memo.Count > 0 then
    if (Memo[0] = '[Image]') then
      if (View is TfrPictureView) then
        begin
          (View as TfrPictureView).Stretched:=
                 ((imAward.Picture.Height > (View as TfrPictureView).Height) or
                  (imAward.Picture.Width > (View as TfrPictureView).Width));
          (View as TfrPictureView).Picture.Assign(imAward.Picture);
        end;
end;

procedure TfmMain.frUserDatasetCheckEOF(Sender: TObject; var Eof: Boolean);
begin
  Eof:= Count > edLastNumber.Value;
end;

procedure TfmMain.frUserDatasetFirst(Sender: TObject);
begin
  Count:= edFirstNumber.Value;
end;

procedure TfmMain.frUserDatasetNext(Sender: TObject);
begin
  Inc(Count);
end;

end.

