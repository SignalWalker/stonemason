use nom::{error::VerboseError, Finish};
use stonemason::de::parse::{Parsed, Stone};

mod cli;

fn initialize_tracing(log_filter: &str, log_format: cli::LogFormat) {
    use cli::LogFormat;
    let tsub = tracing_subscriber::fmt()
        .with_writer(std::io::stderr)
        .with_timer(tracing_subscriber::fmt::time::OffsetTime::new(
            time::UtcOffset::current_local_offset().unwrap_or_else(|e| {
                tracing::warn!("couldn't get local time offset: {:?}", e);
                time::UtcOffset::UTC
            }),
            time::macros::format_description!("[hour]:[minute]:[second]"),
        ))
        .with_thread_ids(true)
        .with_thread_names(true)
        .with_env_filter(log_filter);

    match log_format {
        LogFormat::Compact => tsub.compact().init(),
        LogFormat::Full => tsub.init(),
        LogFormat::Pretty => tsub.pretty().init(),
        LogFormat::Json => tsub.json().init(),
    }
}

fn main() {
    let args = <cli::Cli as clap::Parser>::parse();
    initialize_tracing(&args.log_filter, args.log_format);

    for path in args.files {
        let data = std::fs::read_to_string(&path).unwrap();
        match Stone::from_parse::<VerboseError<&str>>(&data) {
            Ok((unconsumed, stn)) => {
                tracing::debug!(?path, "{stn:#?}");
                print!("{stn}");
            }
            Err(e) => match e {
                nom::Err::Incomplete(e) => tracing::error!(?path, "Incomplete: need {:?} bytes", e),
                nom::Err::Error(e) | nom::Err::Failure(e) => {
                    tracing::error!(?path, "{}", nom::error::convert_error(data.as_str(), e))
                }
            },
        }
    }
}
