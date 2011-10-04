unit PreviewFrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, LR_View, lr_e_pdf, PrintersDlgs;

type

  { TPreviewDialog }

  TPreviewDialog = class(TForm)
    btClose: TButton;
    btSave: TButton;
    btPrint: TButton;
    frPreview: TfrPreview;
    pnlStatusBar: TPanel;
    pbrProgress: TProgressBar;
    PrintDialog: TPrintDialog;
    SaveDialog: TSaveDialog;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  PreviewDialog: TPreviewDialog;

implementation

{$R *.lfm}

end.

