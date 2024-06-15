const path = require("path");
const CopyPlugin = require("copy-webpack-plugin");
const WasmPackPlugin = require("@wasm-tool/wasm-pack-plugin");
const webpack = require("webpack");
const sveltePreprocess = require("svelte-preprocess");

const dist = path.resolve(__dirname, "dist");

module.exports = {
    // mode: "production",
    mode: "development",
    experiments: {
        asyncWebAssembly: true,
    },
    devtool: 'eval-cheap-module-source-map',
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
                test: /\.(html|svelte)$/,
                use: {
                    loader: "svelte-loader",
                    options: {
                        preprocess: sveltePreprocess(),
                    },
                },
            },
            {
                // required to prevent errors from Svelte on Webpack 5+, omit on Webpack 4
                test: /node_modules\/svelte\/.*\.mjs$/,
                resolve: {
                    fullySpecified: false,
                },
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
        alias: {
            svelte: path.resolve("node_modules", "svelte/src/runtime"), // Svelte 3: path.resolve('node_modules', 'svelte')
        },
        extensions: [".mjs", ".js", ".svelte", ".ts", ".tsx"],
        mainFields: ["svelte", "browser", "module", "main"],
        conditionNames: ["svelte", "browser", "import"],
    },
    entry: {
        index: "./js/index.js",
        manual: "./js/manual.js",
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

        new webpack.LoaderOptionsPlugin({
            options: {
                experiments: {
                    asyncWebAssembly: true,
                },
            },
        }),
    ],
};
