/// If this match consumer the tokens
/// If it does not match, do not consume the tokens,
/// and exeucte the statement.
macro_rules! parse_expect_or {
    ($self:ident, $x:pat, $y: stmt) => {
        let Some($x) = $self.peek() else { $y };
        $self.next();
    };
}

/// If the sequence matches, consume the tokens
/// If it does not match, do not consume the tokens
macro_rules! parse_sequence {
    ($self:ident, $(($($x:pat $(if $opt_ex:expr)?),*) => $y: expr),*) => {
        $(
            'block: {
                let mut count = 0;
                $(
                  #[allow(unused_assignments, unused_variables)]
                    if let Some($x) = $self.peek_forward(count) {
                        $(
                            if !$opt_ex {
                                break  'block;
                            }
                        )?
                        count += 1;
                    } else {
                        break  'block;
                    }

                )*
                $(
                    let Some($x) = $self.next() else {
                         panic!("This should not happen")
                    };
                )*
                $y
            }
        )*
    };
}
