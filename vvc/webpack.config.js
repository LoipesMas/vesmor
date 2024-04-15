const path = require("path");
const CopyPlugin = require("copy-webpack-plugin");
const WasmPackPlugin = require("@wasm-tool/wasm-pack-plugin");
const webpack = require("webpack");

const dist = path.resolve(__dirname, "dist");

module.exports = {
    // mode: "production",
    mode: "development",
    experiments: {
        asyncWebAssembly: true,
    },
    module: {
        rules: [
            {
                test: /\.(ts|tsx)$/,
                exclude: /node_modules/,
                use: "ts-loader",
            },
            {
                test: /\.wasm$/,
                type: "webassembly/async",
            },
            {
                test: /\.js$/,
                exclude: /node_modules/,
                use: "babel-loader",
            },
            {
                test: /\.s[ac]ss$/i,
                use: [
                    // Creates `style` nodes from JS strings
                    "style-loader",
                    // Translates CSS into CommonJS
                    "css-loader",
                    // Compiles Sass to CSS
                    "sass-loader",
                ],
            },
        ],
    },
    resolve: {
        extensions: [".ts", ".tsx", ".js"],
    },
    entry: {
        index: "./js/index.js",
        manual: "./js/manual.ts",
    },
    output: {
        hashFunction: "xxhash64",
        path: dist,
        filename: "[name].js",
    },
    devServer: {
        static: [dist],
        client: {
            overlay: {
                runtimeErrors: false,
            },
        },
    },
    performance: { hints: false },
    plugins: [
        new CopyPlugin({
            patterns: [path.resolve(__dirname, "static")],
        }),

        new WasmPackPlugin({
            crateDirectory: __dirname,
        }),
        new webpack.LoaderOptionsPlugin({
            options: {
                experiments: {
                    asyncWebAssembly: true,
                },
            },
        }),
    ],
};
