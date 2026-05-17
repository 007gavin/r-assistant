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
| **Viewer 面板聊天** | `addin_chat()` | ✅ **新增** |
| **模型选择** | `assistant_select_model()` | ✅ **新增** |
| **历史记录** | `assistant_history()` | ✅ **新增** |

## 支持的 LLM 提供商

- **DeepSeek** — DeepSeek Chat, DeepSeek Coder（国内直连，推荐）
- **OpenAI** — GPT-4o, GPT-4o-mini, GPT-4-turbo
- **Anthropic** — Claude Sonnet, Claude Haiku, Claude Opus
- **OpenRouter** — 通过 OpenRouter 访问多种模型
- **Custom** — 任何 OpenAI 兼容 API

## 安装

### 从 GitHub 安装（推荐）

```r
install.packages("remotes")
remotes::install_github("007gavin/r-assistant")
```

### 从本地安装

```r
# 从打包文件安装
install.packages("D:/r-assistant/r.assistant_0.3.0.tar.gz", repos = NULL, type = "source")
```

## 快速开始

### 1. 配置 API

```r
library(r.assistant)

# 设置提供商和 API Key
assistant_config(provider = "deepseek", api_key = "sk-xxx")

# 查看当前配置
assistant_get_config()
```

### 2. 使用 Viewer 面板聊天

```r
# 打开聊天面板（在 RStudio Viewer 面板中显示，Console 完全空闲）
addin_chat()

# 关闭聊天
addin_chat_close()
```

聊天面板功能：
- **模型选择器** — 顶部下拉框，点击切换 LLM 模型
- **历史按钮** — 时钟图标，查看对话历史
- **新对话** — + 按钮，清空并开始新对话
- **插入代码** — 文件图标，将 AI 代码插入编辑器

### 3. 日常使用

```r
# 对话式问答
assistant_chat("如何用 ggplot2 画箱线图？")
assistant_chat("再加上颜色分组")

# 快速提问
assistant_ask("lapply 和 sapply 的区别是什么？")

# 解释选中的代码
assistant_explain()

# 代码重构
assistant_refactor("for(i in 1:nrow(df)) { df$x[i] <- df$y[i] * 2 }")

# 错误修复
assistant_fix()

# 生成文档
assistant_document("my_func <- function(x, y = 10) { x + y }")

# 生成单元测试
assistant_test("add <- function(a, b) a + b")
```

### 4. 模型选择

```r
# 查看当前提供商的可用模型
assistant_list_models()

# 交互式选择模型
assistant_select_model()

# 命令行直接切换
assistant_set_model("deepseek-coder")
```

### 5. 历史记录

```r
# 查看最近 10 条对话
assistant_history(n = 10)

# 清空历史
assistant_clear_history()
```

## RStudio Addin 使用

安装后在 RStudio 菜单栏 **Addins** 中可以看到：

- **R Assistant Chat** — 打开 Viewer 面板聊天
- **Explain Code** — 解释选中的代码
- **Refactor Code** — 重构选中的代码
- **Fix Code** — 修复选中代码的错误
- **Generate Docs** — 为选中函数生成文档

建议设置键盘快捷键：`Tools → Modify Keyboard Shortcuts → Addins`

## 上下文感知

R Assistant 会自动收集以下上下文信息：

- R 版本和操作系统
- 已加载的 R 包
- 全局环境中的变量（名称、类型、维度）
- RStudio 中当前打开的文件内容
- 当前选中的代码

## 配置详解

```r
assistant_config(
  provider         = "deepseek",      # LLM 提供商
  model            = "deepseek-chat",  # 模型名称
  api_key          = "sk-xxx",         # API 密钥
  base_url         = "",               # 自定义 API 地址
  temperature      = 0.3,              # 生成温度 (0-2)
  max_tokens       = 4096,             # 最大回复长度
  system_prompt    = "...",            # 自定义系统提示词
  context_enabled  = TRUE              # 是否包含 R 会话上下文
)
```

## 项目结构

```
D:/r-assistant/
├── DESCRIPTION          # 包元数据 (v0.3.0)
├── NAMESPACE            # 导出/导入声明
├── LICENSE              # MIT 许可证
├── README.md            # 本文档
├── R/
│   ├── api.R            # API 通信核心
│   ├── config.R         # 配置管理
│   ├── context.R        # 上下文采集
│   ├── assistant.R      # 主功能函数
│   ├── history.R        # 对话历史管理
│   ├── models.R         # 模型选择功能
│   └── addin.R          # RStudio Addin + Viewer 面板
├── man/                 # 自动生成的文档
└── inst/
    ├── manual.html      # HTML 使用手册
    └── rstudio/
        └── addins.dcf   # Addin 注册文件
```

## 版本历史

| 版本 | 日期 | 更新内容 |
|------|------|----------|
| v0.3.0 | 2026-05-17 | 模型选择器、历史记录查看、新函数 |
| v0.2.2 | 2026-05-17 | Viewer 面板显示优化 |
| v0.2.0 | 2026-05-17 | 后台进程运行，Console 不阻塞 |
| v0.1.0 | 2026-05-17 | 初始版本，核心功能 |

## 许可证

MIT License
