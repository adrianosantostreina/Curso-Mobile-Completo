unit Constantes;

interface

type
  TTipoValidacao = (tvCPF, tvEmail);


const

  {Dados comuns REST}
  //C_BASEURL                 = 'http://tdevrocks.ddns.com.br:58050/datasnap/rest/';
  //C_BASEURL                   = 'http://192.168.1.40:8080/datasnap/rest/';
  C_BASEURL                   = 'http://localhost:8080/datasnap/rest/';

  //ServerMethods
  C_METODOSGERAIS             = 'TSrvMetodosGerais';
  C_SmServicos                = 'TSmServicos';
  C_SM_ESTABELECIMENTOS       = 'TsmEstabelecimentos';

  //Recursos
  C_ESTABELECIMENTOS          = 'estabelecimentos';
  C_CATEGORIAS                = 'categorias';
  C_CARDAPIOS                 = 'cardapios';
  C_PERFIL                    = 'perfil';
  C_USUARIOS                  = 'usuarios';
  C_LOGIN                     = 'login';
  C_PEDIDOS                   = 'pedidos';
  C_PEDIDOS_ESTABELECIMENTO   = 'pedidosestabelecimento';
  C_ITENS_PEDIDOS             = 'itenspedido';
  C_ATUALIZARSTATUS           = 'AtualizarStatus';

  {Expressões Regulares}
  C_EXP_CPF        = '([0-9]{2}[\.]?[0-9]{3}[\.]?[0-9]{3}'     +
                     '[\/]?[0-9]{4}[-]?[0-9]{2})|([0-9]{3}'    +
                     '[\.]?[0-9]{3}[\.]?[0-9]{3}[-]?[0-9]{2})' ;

  C_EXP_EMAIL      = '^((?>[a-zA-Z\d!#$%&''*+\-/=?^_`{|}~]+\x20*' +
                     '|"((?=[\x01-\x7f])[^"\\]|\\[\x01-\x7f])*"\' +
                     'x20*)*(?<angle><))?((?!\.)(?>\.?[a-zA-Z\d!' +
                     '#$%&''*+\-/=?^_`{|}~]+)+|"((?=[\x01-\x7f])' +
                     '[^"\\]|\\[\x01-\x7f])*")@(((?!-)[a-zA-Z\d\' +
                     '-]+(?<!-)\.)+[a-zA-Z]{2,}|\[(((?(?<!\[)\.)' +
                     '(25[0-5]|2[0-4]\d|[01]?\d?\d)){4}|[a-zA-Z\' +
                     'd\-]*[a-zA-Z\d]:((?=[\x01-\x7f])[^\\\[\]]|' +
                     '\\[\x01-\x7f])+)\])(?(angle)>)$';

  implementation


end.
