unit uFormSimuladorHTTP;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Generics.Collections, System.DateUtils, StrUtils,
  System.JSON;

type
  TProduto = record
    Nome: String;
    Valor: Currency;
  end;

  Constante = record
    Const
      URL = 'https://api.com/produto';
  end;

  TForm1 = class(TForm)
    Metodo: TComboBox;
    LMetodo: TLabel;
    URL: TEdit;
    LURL: TLabel;
    Executar: TButton;
    Body: TMemo;
    LBody: TLabel;
    Request: TMemo;
    LRequest: TLabel;
    Response: TMemo;
    LResponse: TLabel;
    Key: TEdit;
    LHeader: TLabel;
    LKey: TLabel;
    Edit1: TEdit;
    LValue: TLabel;
    procedure ExecutarClick(Sender: TObject);
  private
    Produto: TDictionary<Integer,TProduto>;

    function GetUTC(const prDate: TDateTime): String;
    function GetJsonProduto(): String;

    procedure CadastrarProduto(); 
    procedure DeletarProduto();
    procedure MetodoNaoSuportado();
    procedure ResponseGet();
    procedure RequestGet();
    procedure ResponsePost();
    procedure RequestPost();
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TForm1 }

procedure TForm1.CadastrarProduto;
var
  vJson: TJSONObject;
  vKey: Integer;
  vID: Integer;
  vProduto: TProduto;
begin
  vID := 0;
  
  vJson := TJSONObject.ParseJSONValue( Body.Text) as TJSONObject;

  for vKey in Produto.Keys do
    if vKey > vID then
      vID := vKey; 

  vProduto.Nome := vJson.GetValue('nome').Value;
  vProduto.Valor := StrToCurr(vJson.GetValue('valor').Value);  
  Produto.Add(vID + 1, vProduto);
end;

constructor TForm1.Create(AOwner: TComponent);
var
  vProduto: TProduto;
begin
  inherited;
  Metodo.AddItem('GET',nil);
  Metodo.AddItem('POST',nil);
  Metodo.AddItem('DELETE',nil);

  Produto := TDictionary<Integer,TProduto>.Create;
  vProduto.Nome := 'Produto';
  vProduto.Valor := 10.50;
  Produto.Add(1, vProduto);

  vProduto.Nome := 'Produto teste';
  vProduto.Valor := 200.35;
  Produto.Add(2, vProduto);
end;

procedure TForm1.DeletarProduto;
begin

end;

procedure TForm1.ExecutarClick(Sender: TObject);
begin
  if URL.Text = Constante.URL then
  begin
    if Metodo.Text = 'GET' then
    begin
      ResponseGet();
      RequestGet();
    end
    else
      if Metodo.Text = 'POST' then
        CadastrarProduto()
    else
      if Metodo.Text = 'DELETE' then
        DeletarProduto()
    else
      MetodoNaoSuportado();
  end;

end;

function TForm1.GetJsonProduto: String;
var
  vKey: Integer;
  vCount: Integer;
  vResult: String;
begin
  vResult := '[' + sLineBreak;
  for vKey in Produto.Keys do
  begin
    Inc(vCount);
    vResult := vResult + '  {' + sLineBreak;
    vResult := vResult + '    "nome": "' + Produto.Items[vKey].Nome + '"' + sLineBreak;
    vResult := vResult + '    "valor": "' + CurrToStr(Produto.Items[vKey].Valor) + '"' + sLineBreak;
    if Produto.Count = vCount then
      vResult := vResult + '  }' + sLineBreak
    else
      vResult := vResult + '  },' + sLineBreak;
  end;
  vResult := vResult + ']';

  Result := vResult;
end;

function TForm1.GetUTC(const prDate: TDateTime): String;
var
  vResult: String;     
begin
  case DayOfTheWeek(prDate) of
    DayMonday: vResult := 'Mon';
    DayTuesday: vResult := 'Tue';
    DayWednesday: vResult := 'Wed';
    DayThursday: vResult := 'Thu';
    DayFriday: vResult := 'Fri';
    DaySaturday: vResult := 'Sat';
    DaySunday: vResult := 'Sun';
  end; 

  vResult := vResult + ', ' + IntToStr(DayOf(prDate));

  case MonthOf(prDate) of
    MonthJanuary: vResult := vResult + ' Jan';
    MonthFebruary: vResult := vResult + ' Feb';
    MonthMarch: vResult := vResult + ' Mar';
    MonthApril: vResult := vResult + ' Apr';
    MonthMay: vResult := vResult + ' May';
    MonthJune: vResult := vResult + ' Jun';
    MonthJuly: vResult := vResult + ' Jul';
    MonthAugust: vResult := vResult + ' Aug';
    MonthSeptember: vResult := vResult + ' Sep';
    MonthOctober: vResult := vResult + ' Oct';
    MonthNovember: vResult := vResult + ' Nov';
    MonthDecember: vResult := vResult + ' Dec';
  end; 
  
  vResult := vResult + ' ' + IntToStr(YearOf(prDate));
  vResult := vResult + ' ' + IfThen(HourOf(prDate) < 10, '0' + IntToStr(HourOf(prDate)), IntToStr(HourOf(prDate))) + ':' + 
                             IfThen(MinuteOf(prDate) < 10, '0' + IntToStr(MinuteOf(prDate)), IntToStr(MinuteOf(prDate))) + ':' +
                             IfThen(SecondOf(prDate) < 10, '0' + IntToStr(SecondOf(prDate)), IntToStr(SecondOf(prDate))) + ':' + ' GMT';
  
  Result := vResult;   
end;


procedure TForm1.MetodoNaoSuportado;
begin

end;

procedure TForm1.RequestGet;
begin

end;

procedure TForm1.RequestPost;
begin

end;

procedure TForm1.ResponseGet;
var
  vResponse: String;
  vJson: String;
begin
  vJson := GetJsonProduto();
  
  vResponse := vResponse + 'HTTP/1.1 200 OK' + sLineBreak;
  vResponse := vResponse + 'Date: ' + GetUTC(Now) + sLineBreak;
  vResponse := vResponse + 'Content-Type: application/json; charset=utf-8' + sLineBreak;
  vResponse := vResponse + 'Content-Length: ' + IntToStr(Length(vJson)) + sLineBreak;
  vResponse := vResponse + 'Connection: keep-alive' + sLineBreak;
  vResponse := vResponse + 'Vary: Accept-Encoding' + sLineBreak;
  vResponse := vResponse + 'Content-Encoding: br' + sLineBreak;
  vResponse := vResponse + sLineBreak + vJson;

  Response.Text := vResponse;
end;

procedure TForm1.ResponsePost;
begin

end;

end.
