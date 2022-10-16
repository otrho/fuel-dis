use colorful::{Colorful, RGB};

pub(crate) enum Markup {
    Empty,
    Space,
    Text(String),
    Indent(Box<Markup>),
    Padded(Box<Markup>, usize),
    Color(Box<Markup>, RGB),
    Pair(Box<Markup>, Box<Markup>),
    SepList(Vec<Markup>, Box<Markup>),
}

impl Markup {
    pub(crate) fn text<S: Into<String>>(s: S) -> Self {
        Markup::Text(s.into())
    }

    pub(crate) fn indented(self) -> Self {
        Markup::Indent(Box::new(self))
    }

    pub(crate) fn padded(self, width: usize) -> Self {
        Markup::Padded(Box::new(self), width)
    }

    pub(crate) fn colored(self, rgb: RGB) -> Self {
        Markup::Color(Box::new(self), rgb)
    }

    pub(crate) fn append(self, other: Self) -> Self {
        Markup::Pair(Box::new(self), Box::new(other))
    }

    pub(crate) fn sep(items: Vec<Markup>, sep: Self) -> Self {
        Markup::SepList(items, Box::new(sep))
    }

    fn len(&self) -> usize {
        match self {
            Markup::Empty => 0,
            Markup::Space => 1,
            Markup::Text(text) => text.len(),
            Markup::Indent(child) => 4 + child.len(),
            Markup::Padded(child, width) => std::cmp::max(child.len(), *width),
            Markup::Color(child, _) => child.len(),
            Markup::Pair(car_child, cdr_child) => car_child.len() + cdr_child.len(),
            Markup::SepList(children, sep) => {
                children.iter().map(|ch| ch.len()).sum::<usize>() + (children.len() - 1) * sep.len()
            }
        }
    }

    pub(crate) fn render(self) -> String {
        match self {
            Markup::Empty => "".to_string(),
            Markup::Space => ' '.to_string(),
            Markup::Text(text) => text,
            Markup::Indent(child) => format!("    {}", child.render()),
            Markup::Padded(child, width) => {
                let len = child.len();
                if len >= width {
                    child.render()
                } else {
                    format!(
                        "{}{}",
                        child.render(),
                        String::from_iter(std::iter::repeat(' ').take(width - len))
                    )
                }
            }
            Markup::Color(child, color) => child.render().color(color).to_string(),
            Markup::Pair(car_child, cdr_child) => {
                format!("{}{}", car_child.render(), cdr_child.render())
            }
            Markup::SepList(children, sep) => children
                .into_iter()
                .map(|ch| ch.render())
                .collect::<Vec<_>>()
                .join(sep.render().as_str()),
        }
    }
}

