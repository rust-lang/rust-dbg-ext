SCRIPT_DIR="$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
OUTPUT_DIR="$SCRIPT_DIR/../output"

mkdir -p "$OUTPUT_DIR/target"

cd $SCRIPT_DIR/../test-framework
cargo build

cd $SCRIPT_DIR

export RUST_LOG=debug
export RUST_BACKTRACE=1

../test-framework/target/debug/dbt \
    --cargo-workspace $SCRIPT_DIR \
    --debugger gdb \
    --cargo-target-directory "$OUTPUT_DIR/target" \
    --output $SCRIPT_DIR/../output \
    --cargo-profile debug \
    --cargo-profile release
