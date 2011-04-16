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
  Classes, SysUtils, Forms, Dialogs, StdCtrls,
  ExtCtrls, EditBtn, Spin, LR_Class, LR_DSet, lr_e_pdf;

type

  { TfmMain }

TfmMain = class(TForm)
    btCreate: TButton;
    btAbout: TButton;
    btClose: TButton;
    edAward: TLabeledEdit;
    edDate: TDateEdit;
    edImageFile: TFileNameEdit;
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
    pnImage: TPanel;
    procedure btAboutClick(Sender: TObject);
procedure btCloseClick(Sender: TObject);
    procedure btCreateClick(Sender: TObject);
    procedure edImageFileAcceptFileName(Sender: TObject; var Value: String);
    procedure FormShow(Sender: TObject);
    procedure frReportEnterRect(Memo: TStringList; View: TfrView);
    procedure frReportGetValue(const ParName: String; var ParValue: Variant);
    procedure frUserDatasetCheckEOF(Sender: TObject; var Eof: Boolean);
    procedure frUserDatasetFirst(Sender: TObject);
    procedure frUserDatasetNext(Sender: TObject);
  private
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

procedure TfmMain.btCreateClick(Sender: TObject);
var
  error: Integer;
begin
  error:= 0;

  //testa se existe prêmio
  if Length(edAward.Text) < 1 then
    error:= 1;

  //testa se o número final da rifa é maior do que o inicial
  if edLastNumber.Value < edFirstNumber.Value then
    error:= 2;

  //testa se a quantidade de rifas é múltiplo da quantidade de responsáveis
  if mmPeople.Lines.Count > 0 then
    if ((edLastNumber.Value - edFirstNumber.Value + 1) mod mmPeople.Lines.Count) > 0 then
      error:= 3;

  case error of
    1:
      begin
        ShowMessage(sAwardCantBeBlank);
        edAward.SetFocus;
      end;
    2:
      begin
        ShowMessage(sTicketNumbersInvalid);
        edLastNumber.SetFocus;
      end;
    3:
      begin
        ShowMessage(sTicketsQuantityNotMultiple);
        edLastNumber.SetFocus;
      end;
    else
      Generate; // generate the tickets
  end;
end;

procedure TfmMain.edImageFileAcceptFileName(Sender: TObject; var Value: String);
begin
  imAward.Picture.LoadFromFile(Value);
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
    ParValue:= Format('%s: %m', [sPriceCaption, edPrice.Value]);
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

