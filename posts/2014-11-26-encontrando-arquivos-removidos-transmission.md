---
title: Encontrando arquivos removidos do Transmission
tags: perl, script, transmission, torrent
---

Como 50 linhas de Perl salvaram o dia mais uma vez.

Tenho um [Raspberry PI](http://www.raspberrypi.org/) em meu escritório com o
programa de bittorrent [Transmission](https://www.transmissionbt.com/) rodando
dia e noite, conectado a um HD externo. Por um descuido meu, os downloads
acabaram lotando o HD externo.

<!--more-->

Pois bem, como havia muitos downloads que eu não precisava mais, fui removendo
cada torrent utilizando o aplicativo para Android do Transmission, marcando
sempre a opção de remover também os arquivos associados a eles. No entanto, por
algum motivo, isso não funcionou. Os torrents foram removidos do Transmission,
mas os arquivos continuaram no HD, e o espaço livre continuava 0 bytes.

E neste momento eu tinha um problema... porque eu já havia removido cerca de
60Gb que continuavam lá, ocupando meu precioso espaço em disco. Como fazer para
apenas esses arquivos já desassociados do Transmission serem excluídos de fato?

Eu precisava então de duas listagens: a primeira, com todos os arquivos dentro
da pasta de downloads, recursivamente; segundo, os arquivos de todos os
torrents que o Transmission conhecia, pois estes eu precisava manter. Fazendo a
diferença da primeira com a segunda, eu encontraria a listagem de arquivos da
pasta de downloads que o Transmission não conhecia *mais*, e que portanto eu
poderia apagar.

Primeiro tentei investigar `transmission-remote`, mas ele não retornava os
arquivos de forma que eu pudesse manipular de forma fácil.

Assim, apelei para o bom e velho Perl.

Segue a primeira função então, para buscar todos os arquivos da pasta de
downloads, recursivamente:

~~~~~~ {.perl}
use File::Find;

sub all_files_in_folder {
    my ($directory) = @_;

    my @result;

    find(sub {
        my $filename = $File::Find::name;
        push @result, $filename if -f $filename;
    }, $directory);

    return @result;
}
~~~~~~

Depois disso, preciso da listagem dos arquivos que o Transmission ainda conhece.

Há dois módulos que acessavam a interface remota do Transmission:
[P2P::Transmission::Remote](https://metacpan.org/pod/P2P::Transmission::Remote)
e [Transmission::Client](https://metacpan.org/pod/Transmission::Client). Acabei
optando por esta última, porque aquela não me dava um acessor para `files`.

~~~~~~ {.perl}
use Transmission::Client;
use File::Spec::Functions;

sub all_torrent_files {
    my $client = Transmission::Client->new(
        username => 'my-username',
        password => '123456',
        url      => 'http://1.2.3.4:9091/transmission/rpc',
        autodie  => 1
    );

    my %result;

    for my $torrent ( @{ $client->torrents } ) {
        for my $file ( @{ $torrent->files } ) {
            my $key = catfile( $torrent->download_dir, $file->name );

            $result{$key} = 1;
        }
    }

    return %result;
}
~~~~~~

Note que aqui retorno um hash `%result`, com todas as chaves associadas ao valor
`1`, porque quero apenas verificar se uma determinada chave está no hash. Se eu
utilizasse array, precisaria percorrer o array toda vez que fosse procurar por
um nome de arquivo, o que seria muito mais lento.

Finalmente, resta apenas chamar as funções:

~~~~~~ {.perl}
my @folder        = all_files_in_folder('/path/to/downloads');
my %torrent_files = all_torrent_files();

for my $file (@folder) {

    # se o arquivo não está na listagem de torrents
    if (!$torrent_files{$file}) {
        say "rm '$file'"; # não remove, apenas escreve na tela
    }

}
~~~~~~

Para remover de fato, poderíamos substituir o `say` por `unlink $file`.

Isto evidentemente gerará uma série de diretórios vazios, porque a remoção não
é recursiva, é apenas dos arquivos. Para remover os diretórios vazios depois,
pode-se utilizar o comando `find`:


~~~~~~ {.bash}
    $ find /path/to/downloads -empty -type d -delete
~~~~~~
