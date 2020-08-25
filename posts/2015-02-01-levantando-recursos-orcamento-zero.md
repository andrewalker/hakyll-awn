---
title: Levantando Recursos com Orçamento Zero
tags: perl, script, transmission, torrent
---

A história de como montei um sistema de pagamentos sob medida, usando as
melhores ferramentas disponíveis *(sic)* com um custo inicial **zero**.

Tudo começa com um menosprezo social pelo trabalho (e estudos) de minha irmã
caçula...

<!--more-->

Minha irmã [Lilian](https://www.facebook.com/liliancwalker) e sua amiga
[Júlia](https://www.facebook.com/juliastradiotto) estudam Artes Visuais na
UNICAMP. Recentemente, ganharam uma bolsa de estudos para estudar na
Universidade do Porto, em Portugal.

Sabe como um monte de gente tem ido nestes últimos anos para o Ciências sem
Fronteiras, ganhando uma série de
[bolsas e benefícios](http://www.cienciasemfronteiras.gov.br/web/csf/valores-de-auxilios-e-bolsas)
além da vaga na universidade no exterior? Pois é, não é o caso da minha irmã. :(

Infelizmente, Artes Visuais não são contempladas por programas como o CsF, e há
muito menos bolsas disponíveis. O que significa que a estadia das duas ficaria
por conta delas, e bancar todas as contas em Euros não é fácil.

Daí a ideia de se montar um site para "rifar" 2 trabalhos de cada uma. Cada
pessoa escolhe o número de "cupons" que quer comprar, isto é, podendo aumentar
a probabilidade de ganhar. Se por outro lado, não tiver condições de contribuir
muito, compra apenas 1 cupom apostando em apenas 1 trabalho. Cada cupom
custaria R$ 5,00.

**Resultado final**: [código fonte no GitHub](https://github.com/andrewalker/p5-porto6-web)

(Originalmente foi publicado em seismesesnoporto.ga, mas já está fora do ar há
vários anos)

Caso queira saber como foi criado, continue lendo :)

## Desenvolvendo o site - interface visual

Eu não sou nenhum artista, e não tínhamos muito tempo. Como o orçamento era
zero, procuramos templates HTML gratuitos e bonitos. Depois de algumas
pesquisas, encontramos
[Brushed](https://www.alessioatzeni.com/blog/brushed-template/).

A partir do template, trocamos as imagens do slideshow por imagens de Porto,
refizemos os textos, e trocamos o formulário de contato por um de pagamento. O
site inteiro foi feito de forma estática, em HTML puro, e ele interagiria com
uma aplicação que salvaria os dados e redirecionaria para o gateway de
pagamento.

Embora esta parte tenha gastado tempo significativo, não há muito mais de
interessante para contar. É trabalhoso, mas não é meu forte, e há vários
lugares que poderiam receber uma bela refatoração e otimização. Se algum
designer ou programador front-end analisar meu código desta parte, peço perdão
desde já :P

## Desenvolvendo a aplicação back-end

Sem surpresas por aqui - utilizei a fantástica framework MVC
[Catalyst](https://metacpan.org/pod/Catalyst) para desenvolver a aplicação com
um banco de dados PostgreSQL, versionado por [sqitch](https://sqitch.org),
acessado por [DBIx::Class](https://metacpan.org/pod/DBIx::Class).

A aplicação teria o propósito de:

 - Salvar os dados do "comprador";
 - Redirecioná-lo para a página de pagamentos;
 - Alterar status dos pagamentos após confirmação do intermediador (eg. PayPal);
 - Enviar e-mails com a confirmação de recebimento da rifa;

Você pode checar o resultado final aqui:
[https://github.com/andrewalker/p5-porto6-web](https://github.com/andrewalker/p5-porto6-web).

O fluxo completo, então, ficou assim:

 - O usuário submete o formulário com os dados;
   - uma requisição ajax é feita para [/place\_order](https://github.com/andrewalker/p5-porto6-web/blob/master/lib/Porto6/Web/Controller/Checkout.pm);
   - se a requisição foi bem sucedida, o id do pagamento (que é um UUID gerado
pela aplicação) é devolvido para o javascript, que redireciona o usuário para
[/redirect/:payment\_id](https://github.com/andrewalker/p5-porto6-web/blob/master/lib/Porto6/Web/Controller/Redirect.pm);
   - caso o pagamento seja via depósito, em vez disso, o usuário é
redirecionado para [deposit.html](https://github.com/andrewalker/p5-porto6-web/blob/gh-pages/deposit.html);
 - De tempos em tempos, uma tarefa cron aciona o método
[update\_all](https://github.com/andrewalker/p5-porto6-web/blob/master/lib/Porto6/AfterSales.pm#L144),
que atualiza os status dos pagamentos, e outra tarefa cron aciona o método
[send\_payment\_received\_mails](https://github.com/andrewalker/p5-porto6-web/blob/master/lib/Porto6/AfterSales.pm#L226),
que envia e-mails a quem está com status de pago;

De forma que o fluxo dos status é o seguinte:

    new -> waiting -> payed -> sent-email
       \         \
        \         \-> failed
         \
          \-> expired

``new`` é o primeiro status de um pedido, assim que ele foi recebido por nós,
antes de a pessoa ser redirecionada ao intermediador de pagamentos. Quando o
intermediador fica sabendo da existência desse pagamento, o status muda
imediatamente para ``waiting``. Se um status fica em ``new`` por 24 horas, vai
para ``expired``. Por outro lado, o pedido fica em ``waiting`` até que o
intermediador de pagamentos altere seu status.

Por fim, os e-mails são enviados usando
[Email::Sender::Simple](https://metacpan.org/pod/Email::Sender::Manual::QuickStart)
e [Email::MIME](https://metacpan.org/pod/Email::MIME), na classe
[Porto6::Mailer](https://github.com/andrewalker/p5-porto6-web/blob/master/lib/Porto6/Mailer.pm).
Toda a configuração do servidor SMTP a ser usado, bem como as credenciais, são
por meio de variáveis de ambiente.

A comunicação com os intermediadores de pagamento PayPal e PagSeguro são feitas
por meio do bom e velho Business::CPI. Porém, pude perceber que este meu módulo
precisa de bastante atenção :) Fica para um outro post.

## A publicação

Agora a parte boa: como pus tudo isso no ar de graça?

Comecemos pelo domínio: você deve ter se perguntado de onde vem esse ".ga".
Pois bem, é o [domínio de Gabão](https://en.wikipedia.org/wiki/.ga), assim como
o br é do Brasil. Se a falta de conexão do site "Seis Meses no Porto" com Gabão
lhe incomodar, lembre-se de quantos sites não-comerciais usam o .com :P

Enfim, domínios .ga são [oferecidos gratuitamente](http://www.freenom.com/en/index.html).

Além disso, utilizei [CloudFlare](https://www.cloudflare.com/) para gerenciar o domínio. Isto me deu uma série de vantagens:

 - um bom serviço DNS (sempre muito importante, não menospreze esta parte!);
 - [distribuição global](https://pt.wikipedia.org/wiki/Content_Delivery_Network);
 - sistema de cache;
 - certificado SSL gratuito;

De cara, já resolvi um monte de problemas.

O e-mail eu hospedei em um servidor próprio, no mesmo do
[andrewalker.net](https://andrewalker.net). Se eu não tivesse este servidor,
poderia ter utilizado um serviço de hospedagem de e-mails gratuito com domínio
customizado como o [zoho.com](https://www.zoho.com). Isto não é tão fácil de se
encontrar como se pode imaginar, já que Google Apps é pago, e a maioria dos
provedores de e-mail gratuitos não permitem domínios customizados.

A aplicação back-end eu hospedei utilizando [heroku](https://www.heroku.com/),
com o [buildpack do
miyagawa](https://github.com/miyagawa/heroku-buildpack-perl). Isto me
possibilitou SSL entre CloudFlare e a aplicação, para sigilo completo dos
dados, além de um deploy simples e rápido, com [ferramentas ótimas para
relatórios rápidos](https://devcenter.heroku.com/articles/dataclips).

Por fim, os e-mails foram enviados utilizando
[Mandrill](https://mandrillapp.com/), que é gratuito para até 12 mil e-mails
por dia. Diminui drasticamente as chances dos e-mails irem para spam, ou outros
problemas similares. Além de mostrar gráficos legais sobre quem abriu os
e-mails, etc.

As tarefas cron, como para mudar status de pagamentos, etc, eram disparadas de
um servidor próprio, que simplesmente acessavam URL's via wget na aplicação
back-end. Poderia ter sido feito com [Pingdom](https://www.pingdom.com), caso
eu não tivesse o servidor.

Por fim, utilizei [Clicky](https://getclicky.com) para acompanhar o número de
visitantes no site. Mais hipster que Google Analytics :P

Caso queira ganhar um quadro, então, não perca tempo! Dia 07/02 será feito o
sorteio!
