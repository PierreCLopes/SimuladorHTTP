# SimuladorHTTP

CONCEITO DO SIMULADOR

	O simulador de protocolo HTTP do presente projeto, tem como objetivo simular a  troca de mensagens entre o Cliente e Servidor por meio do protocolo HTTP, demonstrando assim as Requests e os Responses.
	Para possibilitar uma simulação realista e não somente a abertura de páginas da Web, o projeto está baseado na comunicação do Cliente com uma API (application
programming interface), ou seja, pelo protocolo HTTP serão simulados alguns recursos de uma API de produtos, podendo ser realizadas buscas (GET), cadastros (POST) e exclusões (DELETE) de produtos, de acordo com o método selecionado.
	O simulador foi desenvolvido para tratar erros de requisições, como Json inválido ou método não suportado, também trata buscas por recursos não presentes na API, ou seja, URLs não esperadas, mas claro, caso a URL esteja no Host esperado.


INSTRUÇÕES

	
Fonte: Elaborado pelo autor.

	Conforme a imagem acima, o simulador possui a estrutura básica para realizar uma requisição na API, tendo a seleção do Método HTTP (GET, POST, DELETE), a URL que por padrão será suportado http://www.api.com.br/produto. Abaixo temos o Header, onde pode ser passado no cabeçalho da mensagem HTTP uma Chave e um Valor dessa chave, por padrão a API irá obrigar um Token, que já é sugerido automaticamente, com a Key “Token” e a Chave “123456789”, caso não seja informada, será retornado erro de autenticação pelo protocolo HTTP. Abaixo temos o Body, onde pode ser informado o corpo da mensagem, de acordo com o método selecionado, será sugerido um formato de corpo para realizar a requisição. Abaixo temos o Request e o Response realizado pelo protocolo HTTP, onde poderá ser visto todas as informações definidas anteriormente.

GET

Fonte: Elaborado pelo autor.

	O método GET irá buscar os produtos cadastrados no servidor, sendo necessário informar a URL padrão (https://www.api.com/produto), o Token de autenticação (123456789) e selecionar o método GET, assim enviando no Request a mensagem HTTP solicitando os produtos, e sendo retornado a mensagem HTTP com status 200 OK, assim estará presente no corpo do Response um Json contendo todos os produtos.

POST

Fonte: Elaborado pelo autor.

	O método POST irá cadastrar um novo produto na API, assim deverá ser preenchidas as informações da mesma forma que foi realizado no GET, porém deverá ser informado o corpo da mensagem (Body) com as informações do novo produto, utilizando a estrutura de Json padrão, que já será sugerida automaticamente. Ao executar a requisição, será enviado o Request com as informações e se todas estiverem corretas, o Response irá retornar com HTTP 200 OK, juntamente com o Json do produto cadastrado contendo o ID do produto na API, porém caso estiver faltando informações ou as informações estiverem incorretas, retornará HTTP 400, ou seja, Bad Request.








DELETE


Fonte: Elaborado pelo autor.

	O método DELETE funciona da mesma forma que o POST, porém o Json enviado conterá apenas o ID do produto que deseja ser excluído, assim ao executar, será enviado o Request com as informações e o Response irá ser retornado da mesma forma que o POST, porém caso o produto não ser encontrado, irá retornar HTTP 404 Not Found.












URL INVÁLIDA


Fonte: Elaborado pelo autor.

	Caso for informada uma URL que não é esperada pelo simulador, por exemplo https://invalido.com.br, será retornado uma mensagem de Erro sem tratamento, pois não existe um servidor tratando a requisição para conseguir informar o HTTP 404, dessa forma somente a URL padrão irá retornar os status HTTP corretamente, seja 200, 400, 401, 404 ou 405.
