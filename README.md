# R Assistant (r.assistant)

一个 R 编程 AI 助手包，类似于 Posit Assistant，支持在 R / RStudio 中进行 AI 辅助编程。

## 功能特性

| 功能 | 函数 | RStudio Addin |
|------|------|---------------|
| 对话式问答 | `assistant_chat()` | ✅ R Assistant Chat |
| 快速提问 | `assistant_ask()` | — |
| 代码解释 | `assistant_explain()` | ✅ Explain Code |
| 代码重构 | `assistant_refactor()` | ✅ Refactor Code |
| 错误修复 | `assistant_fix()` | ✅ Fix Code |
| 文档生成 | `assistant_document()` | ✅ Generate Docs |
| 代码生成 | `assistant_complete()` | — |
| 单元测试生成 | `assistant_test()` | — |

## 支持的 LLM 提供商

- **OpenAI** — GPT-4o, GPT-4o-mini, GPT-4-turbo
- **Anthropic** — Claude Sonnet, Claude Haiku, Claude Opus
- **DeepSeek** — DeepSeek Chat, DeepSeek Coder
- **OpenRouter** — 通过 OpenRouter 访问多种模型
- **Custom** — 任何 OpenAI 兼容 API

## 安装

```r
# 从本地安装
install.packages("D:/r-assistant", repos = NULL, type = "source")

# 或者使用 devtools
# devtools::install("D:/r-assistant")
```

## 快速开始

### 1. 配置 API

```r
library(r.assistant)

# 方法一：直接设置（推荐）
assistant_config(
  provider = "deepseek",
  api_key = "your-api-key-here"
)

# 方法二：分别设置
assistant_set_provider("deepseek")
assistant_set_key("your-api-key-here")

# 方法三：在 RStudio 中使用图形界面配置
assistant_config()  # 弹出配置面板
```

### 2. 日常使用

```r
# 对话式问答
assistant_chat("如何用 ggplot2 画一个带有置信区间的折线图？")

# 多轮对话会自动保持上下文
assistant_chat("再加上不同颜色区分不同组别")

# 快速提问（不保留历史）
assistant_ask("lapply 和 sapply 的区别是什么？")

# 解释选中的代码
# 在 RStudio 中选中代码，然后运行：
assistant_explain()

# 或者直接传入代码
assistant_explain("df %>% group_by(cyl) %>% summarise(across(everything(), mean))")

# 代码重构
assistant_refactor(
  "for(i in 1:nrow(df)) { df$x[i] <- df$y[i] * 2 }",
  style = "tidyverse"
)

# 错误修复
assistant_fix(
  code = "df %>% filter(x = 5)",
  error = "Error in filter(): could not find function '%>%'"
)

# 生成文档
assistant_document("my_summarise <- function(df, group, value) {
  df %>% group_by(across(all_of(group))) %>% summarise(mean = mean(.data[[value]]))
}")

# 生成单元测试
assistant_test("my_func <- function(x) ifelse(x > 0, sqrt(x), NA)")

# 从描述生成代码
assistant_complete("创建一个函数，输入一个数据框和列名，返回该列的正态性检验结果")
```

### 3. RStudio Addin 使用

安装后，在 RStudio 菜单 **Addins** 中可以看到以下工具：

- **R Assistant Chat** — 打开聊天面板
- **Explain Code** — 解释选中的代码
- **Refactor Code** — 重构选中的代码
- **Fix Code** — 修复选中的代码
- **Generate Docs** — 为选中函数生成文档

建议设置键盘快捷键：`Tools → Modify Keyboard Shortcuts → Addins`

## 配置详解

```r
# 查看当前配置
assistant_get_config()

# 完整配置选项
assistant_config(
  provider = "deepseek",        # LLM 提供商
  model = "deepseek-chat",      # 模型名称
  api_key = "sk-xxx",           # API 密钥
  base_url = "",                # 自定义 API 地址（custom provider）
  temperature = 0.3,            # 生成温度 (0-2)，越低越确定
  max_tokens = 4096,            # 最大回复长度
  system_prompt = "...",        # 自定义系统提示词
  context_enabled = TRUE        # 是否自动包含 R 会话上下文
)
```

## 上下文感知

R Assistant 会自动收集以下上下文信息发送给 AI：

- R 版本和操作系统
- 已加载的 R 包
- 全局环境中的变量（名称、类型、维度、大小）
- RStudio 中当前打开的文件内容
- 当前选中的代码

这使得 AI 的回答更加精准和相关。

## 对话历史

```r
# 查看对话历史
assistant_history(n = 5)  # 最近5条

# 清除历史
assistant_clear_history()
```

历史记录保存在 `~/.r-assistant/history.json`。

## 项目结构

```
D:/r-assistant/
├── DESCRIPTION          # 包元数据
├── NAMESPACE            # 导出/导入声明
├── LICENSE              # MIT 许可证
├── README.md            # 本文档
├── DEVELOPMENT_LOG.md   # 开发日志
├── R/
│   ├── api.R            # API 通信核心（提供商注册表、请求构建、响应解析）
│   ├── config.R         # 配置管理（读写、交互式配置界面）
│   ├── context.R        # 上下文采集（会话信息、环境变量、文档内容）
│   ├── assistant.R      # 主功能函数（chat/ask/explain/fix/refactor/document/test）
│   ├── history.R        # 对话历史管理
│   └── addin.R          # RStudio Addin（Shiny gadget 交互界面）
├── man/                 # 自动生成的文档
└── inst/
    └── rstudio/
        └── addins.dcf   # Addin 注册文件
```

## 依赖包

- `httr2` — HTTP 请求
- `jsonlite` — JSON 解析
- `rstudioapi` — RStudio IDE 集成
- `shiny` + `miniUI` — Addin 界面
- `clipr` — 剪贴板操作（可选）

## 许可证

MIT License
