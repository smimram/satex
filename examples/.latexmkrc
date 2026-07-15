add_cus_dep('satex', 'satix', 0, 'generate_satix');

sub generate_satix {
    my $base = $_[0];
    return system("../satex $base.satex");
}

push @generated_exts, 'satex';
push @generated_exts, 'satix';