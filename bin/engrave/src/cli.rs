use std::path::PathBuf;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, clap::ValueEnum)]
pub enum LogFormat {
    Compact,
    Full,
    Pretty,
    Json,
}

impl std::fmt::Display for LogFormat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LogFormat::Compact => f.write_str("compact"),
            LogFormat::Full => f.write_str("full"),
            LogFormat::Pretty => f.write_str("pretty"),
            LogFormat::Json => f.write_str("json"),
        }
    }
}

#[derive(clap::Parser, Debug)]
#[command(version, author, about)]
pub struct Cli {
    /// Logging output filters; comma-separated
    #[arg(
        short,
        long,
        default_value = "warn,engrave=info,stonemason=info",
        env = "ENGRAVE_LOG_FILTER"
    )]
    pub log_filter: String,
    /// Logging output format
    #[arg(long, default_value_t = LogFormat::Pretty)]
    pub log_format: LogFormat,
    #[arg()]
    pub files: Vec<PathBuf>,
}
