// native app entry_point

use async_std::task::block_on;

use sketch::run_app;

mod sketch;

fn main() {
    block_on(async {
        run_app().await;
    });
}
