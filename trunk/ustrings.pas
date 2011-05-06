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

unit uStrings;

{$mode objfpc}{$H+}

interface

resourcestring
  sAboutDialogCaption = 'Sobre %s';
  sProgramInfo = '%s compilado para %s em %s com %s';
                 {'This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.';}
  sLicenseIntro = 'Este programa é software livre: você pode redistribuí-lo e/ou modificá-lo sob os termos da Licença Pública Geral GNU, conforme publicada pela Free Software Foundation; tanto a versão 3 da Licença como (a seu critério) qualquer versão posterior.';

         {'This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.';}
  sAsIs = 'Este programa é distribuído na expectativa de ser útil, mas SEM QUALQUER GARANTIA; incluindo as garantias implícitas de COMERCIALIZAÇÃO ou de ADEQUAÇÃO A QUALQUER PROPÓSITO EM PARTICULAR.';



  sAwardCantBeBlank = 'O prêmio não pode ficar em branco!';
  sTicketNumbersInvalid = 'O número final da rifa deve ser maior ou igual ao número inicial!';
  sTicketsQuantityNotMultiple = 'A quantidade de rifas deve ser múltiplo da quantidade de responsáveis.';

  sNameCaption = 'Nome';
  sPhoneCaption = 'Tel';
  sDateCaption = 'Data';
  sPlaceCaption = 'Local';
  sPriceCaption = 'Valor';

implementation

end.

