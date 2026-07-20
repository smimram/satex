# NOTE: the .satix file is input via \IfFileExists so that latexmk does not
# detect it as a dependency, we use a fake pdflatex in order to register it.

$pdflatex = 'internal satex_latex %R %Z pdflatex %O %S';

sub satex_latex {
    my $root = shift;
    my $dir_string = shift;
    my $ret = system @_;
    my $satex = $dir_string . $root . '.satex';
    my $satix = $dir_string . $root . '.satix';
    if ( (-e $satex) && (-s $satex) ) { rdb_ensure_file( $rule, $satix ); }
    return $ret;
}

add_cus_dep('satex', 'satix', 0, 'generate_satix');

sub generate_satix { return system("../satex \"@_.satex\""); }

push @generated_exts, 'satex';
push @generated_exts, 'satix';
