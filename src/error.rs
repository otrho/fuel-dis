pub(crate) struct Error((String, Option<String>));

impl Error {
    pub(crate) fn new<S: Into<String>>(msg: S, details: Option<S>) -> Self {
        Error((msg.into(), details.map(|s| s.into())))
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        let (msg, details) = &self.0;

        writeln!(f, "error: {msg}")?;

        if let Some(details) = details {
            writeln!(f, "\n{details}")?;
        }

        Ok(())
    }
}
