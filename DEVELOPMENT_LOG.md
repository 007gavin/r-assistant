================================================================================
R Assistant 项目开发日志
================================================================================
项目名称: r.assistant (R AI 编程助手包)
存储路径: D:/r-assistant/
开发者: Hermes Agent (AI) + Gavin
开始时间: 2026-05-17
================================================================================

目录
----
1. 需求分析与设计决策
2. 项目结构设计
3. 开发过程详细记录
4. 文件说明与代码逻辑
5. 测试与验证
6. 后续计划

================================================================================
第1章: 需求分析与设计决策
================================================================================

[2026-05-17 00:00] 用户需求:
  - 创建一个R包/插件，可应用于R Server
  - 调用LLM API实现AI辅助编程
  - 功能类似于Posit Assistant（RStudio内置的AI助手）
  - 项目文件存储于D盘，新建文件夹

[2026-05-17 00:01] 需求分析:
  Posit Assistant的核心功能包括:
  1. 对话式编程辅助 — 用户可以用自然语言描述需求，AI生成R代码
  2. 代码解释 — 选中代码片段，AI解释其功能
  3. 代码重构 — 优化/重写现有代码
  4. 错误诊断 — 分析错误信息并提供修复方案
  5. 文档生成 — 为函数自动生成roxygen2文档
  6. 上下文感知 — 能读取当前R会话状态（已加载包、变量结构等）
  7. RStudio集成 — 通过Addin菜单/快捷键直接调用

[2026-05-17 00:02] 技术方案决策:

  决策1: 使用httr2而非httr或curl
    原因: httr2是httr的现代继任者，API更简洁，内置重试、
          错误处理等。适合高频API调用场景。

  决策2: 支持多个LLM提供商（而非绑定单一API）
    原因: 用户环境可能有不同API访问权限，DeepSeek在国内可直连，
          OpenAI/Anthropic可能需要代理。采用适配器模式，
          每个提供商有独立的header_fn和response解析逻辑。

  决策3: 使用Shiny/miniUI做交互界面
    原因: RStudio的Addin系统原生支持miniUI gadget，
          可以在RStudio内部弹出对话窗，无需外部浏览器。
          对于纯命令行R Server场景，也提供纯文本函数接口。

  决策4: 配置存储使用JSON文件（~/.r-assistant/config.json）
    原因: 跨平台兼容，不需要额外依赖，手动编辑也方便。
          API key存储在本地，不会上传到任何地方。

  决策5: 上下文自动采集
    原因: 类似Posit Assistant的关键特性——AI需要知道用户的
          当前环境（已加载包、变量、选中代码等）才能给出
          最相关的回答。通过rstudioapi和session info自动收集。

================================================================================
第2章: 项目结构设计
================================================================================

[2026-05-17 00:03] 包结构:

D:/r-assistant/
├── DESCRIPTION          # R包元数据（名称、版本、依赖）
├── NAMESPACE            # 导出/导入声明
├── LICENSE              # MIT许可证
├── R/
│   ├── api.R            # 核心API调用模块（请求构建、响应解析）
│   ├── config.R         # 配置管理（provider/model/key/温度等）
│   ├── context.R        # R会话上下文采集（包、变量、选中代码）
│   ├── assistant.R      # 主功能函数（chat/ask/explain/fix/refactor等）
│   ├── history.R        # 对话历史管理（本地JSON存储）
│   └── addin.R          # RStudio Addin注册和Shiny gadget界面
├── man/                 # roxygen2自动生成的文档
├── inst/
│   ├── rstudio/
│   │   └── addins.dcf   # RStudio Addin注册文件
│   └── scripts/         # 辅助脚本
└── README.md            # 使用说明

模块职责划分:
  api.R     — 纯粹的HTTP通信层，不涉及业务逻辑
  config.R  — 配置的读写和验证，provider注册表
  context.R — 从R会话和RStudio采集环境信息
  assistant.R — 业务逻辑层，组合api+context+config完成各种任务
  history.R — 对话历史的持久化和管理
  addin.R   — 用户界面层，Shiny gadget交互

================================================================================
第3章: 开发过程详细记录
================================================================================

[2026-05-17 00:04] === 阶段1: 创建目录结构和元数据 ===

  操作: 创建 D:/r-assistant/ 及子目录 R/ man/ inst/rstudio/ inst/scripts/
  结果: 成功

[2026-05-17 00:05] === 编写 DESCRIPTION ===

  文件: D:/r-assistant/DESCRIPTION
  内容说明:
    - Package: r.assistant
    - 依赖: httr2 (HTTP), jsonlite (JSON), rstudioapi (IDE集成),
            miniUI + shiny (Addin UI), clipr (剪贴板)
    - Suggests: testthat, knitr, rmarkdown (测试和文档)
  决策: 使用Imports而非Depends，避免污染用户命名空间

[2026-05-17 00:06] === 编写 NAMESPACE ===

  文件: D:/r-assistant/NAMESPACE
  内容说明:
    - 导出21个公共函数
    - 从httr2导入核心HTTP函数
    - 从jsonlite导入toJSON/fromJSON
    - 从rstudioapi导入IDE交互函数
  决策: 手写NAMESPACE而非完全依赖roxygen2，确保导入列表精确

[2026-05-17 00:07] === 编写 LICENSE ===

  文件: D:/r-assistant/LICENSE
  许可: MIT（最宽松，方便用户自由使用和修改）

[2026-05-17 00:08] === 编写 R/api.R — API通信核心 ===

  文件: D:/r-assistant/R/api.R
  包含:
    - PROVIDERS 注册表: 定义5个提供商 (openai/anthropic/deepseek/openrouter/custom)
      每个提供商有: name, base_url, models, default_model, api_key_env,
                   chat_path, header_fn
    - build_request_body(): 根据provider格式构造请求体
      ★ 重要差异: Anthropic的API格式与OpenAI不兼容
        - Anthropic: system单独提出来作为顶级字段，不用messages里的system角色
        - OpenAI/DeepSeek/OpenRouter: 统一用OpenAI格式
    - parse_response(): 解析不同格式的响应
      - Anthropic: resp$content[[1]]$text
      - OpenAI兼容: resp$choices[[1]]$message$content
  设计决策: 采用适配器模式而非强制统一，因为各provider API差异较大
            （特别是Anthropic的system prompt处理方式）

[2026-05-17 00:09] === 编写 R/config.R — 配置管理 ===

  文件: D:/r-assistant/R/config.R
  包含:
    - 配置目录: ~/.r-assistant/
    - load_config() / save_config(): JSON文件读写
    - assistant_get_config(): 获取配置（带默认值合并）
    - assistant_config(): 交互式配置（调用时无参数则弹出Shiny gadget）
    - assistant_set_key(): 设置API key（同时存入环境变量和配置文件）
    - assistant_set_model() / assistant_set_provider(): 快速切换
    - default_system_prompt(): 默认系统提示词
      内容: 定义AI的角色（R编程专家）和行为准则
    - .assistant_config_gadget(): Shiny配置界面
  设计决策:
    - 双重存储API key（环境变量+配置文件），环境变量优先级更高
    - 配置使用JSON而非YAML，减少依赖
    - system prompt精心设计，要求AI用```r代码块回复，便于自动提取

================================================================================
第4章: 文件说明与代码逻辑（持续更新）
================================================================================

[2026-05-17 00:10] === 编写 R/context.R — 上下文采集模块 ===

  文件: D:/r-assistant/R/context.R
  包含:
    - assistant_context(): 主函数，采集当前R会话全部上下文
      字段包括:
        * r_version, os, platform — 环境基本信息
        * loaded_packages — 用户加载的包（排除base包）
        * environment — 全局环境变量列表
          每个变量包含: name, class, type, size, dims, preview
          使用str()做preview，object.size()做大小统计
          最多采集30个变量（max_vars参数控制）
        * selected_code — RStudio中选中的代码
        * document_content — 当前打开的文件内容（截断到3000字符）
        * working_directory — 工作目录
    - format_context(): 将上下文列表格式化为文本块
      用于嵌入系统提示词中
      最大4000字符，超出则截断
  设计决策:
    - 上下文采集失败不中断主流程（tryCatch包裹）
    - 文件内容截断3000字符，避免token爆炸
    - 变量preview用str(max.level=0)只取概要，不递归展开
    - 所有采集步骤可通过参数独立开关

[2026-05-17 00:11] === 编写 R/history.R — 对话历史管理 ===

  文件: D:/r-assistant/R/history.R
  包含:
    - HISTORY_FILE: ~/.r-assistant/history.json
    - load_history() / save_history(): JSON文件读写
    - add_to_history(): 添加消息，自动裁剪到最大长度
    - assistant_history(): 查看历史（n参数控制条数，as_messages控制格式）
    - assistant_clear_history(): 清空历史
    - %||% 运算符: null合并操作符
  设计决策:
    - 历史消息包含时间戳，便于调试
    - 默认最多保留50条（可在config中调整）
    - as_messages=TRUE时返回API格式（去掉timestamp），方便直接发给API

[2026-05-17 00:12] === 编写 R/assistant.R — 核心业务逻辑 ===

  文件: D:/r-assistant/R/assistant.R
  包含:
    - .call_llm(): 内部核心函数，所有AI调用的统一入口
      流程: 合并system_prompt+context+history+新消息 → 构建请求 → 调用API → 解析响应 → 保存历史
      错误处理: API key缺失、HTTP错误、网络超时均有明确错误信息
      重试: httr2内置retry，最多3次
      超时: 120秒

    - assistant_chat(): 对话式问答（保留历史，支持多轮）
    - assistant_ask(): 一次性提问（不保留历史）
    - assistant_explain(): 代码解释（支持brief/normal/detailed三级详细度）
    - assistant_refactor(): 代码重构（支持tidyverse/base/data.table风格，readability/performance/conciseness目标）
    - assistant_fix(): 错误修复（自动读取RStudio选中代码和最后一条错误信息）
    - assistant_document(): 文档生成（roxygen2格式）
    - assistant_complete(): 从描述生成代码
    - assistant_test(): 生成单元测试（testthat/tinytest）

    - extract_code_blocks(): 从AI回复中提取```r代码块
    - .get_selection_or_stop(): 获取RStudio选中代码，无选中则报错

  设计决策:
    - .call_llm统一处理所有API调用，避免各函数重复代码
    - assistant_fix自动尝试读取geterrmessage()获取最后一条错误
    - assistant_explain有三级详细度，适配不同使用场景
    - 所有函数都支持不带参数调用（自动使用RStudio选中代码）
    - extract_code_blocks用正则提取代码块，便于后续自动插入编辑器

[2026-05-17 00:13] === 编写 R/addin.R — RStudio Addin界面 ===

  文件: D:/r-assistant/R/addin.R
  包含:
    - addin_chat(): 聊天Addin（完整Shiny gadget）
      特性:
        * 聊天气泡式界面（用户蓝色，助手灰色）
        * 自动将RStudio选中代码作为首轮消息发送
        * "Thinking..."加载指示器
        * 基础Markdown转HTML（代码块、行内代码）
        * Clear按钮清空聊天和历史
        * 支持paneViewer（嵌入RStudio面板）

    - addin_explain(): 解释选中代码（调用assistant_explain）
    - addin_refactor(): 重构选中代码
    - addin_fix(): 修复选中代码
    - addin_document(): 为选中函数生成文档

    - .show_result_gadget(): 在Shiny gadget中显示结果文本
    - .offer_code_insertion(): 代码插入辅助
      选项: 替换选中代码 / 插入到选中代码之后 / 复制到剪贴板 / 仅显示

  设计决策:
    - 使用miniUI而非完整Shiny app，符合RStudio Addin规范
    - paneViewer而非dialogViewer，聊天面板可以常驻侧边
    - 提供代码插入选项而非自动插入，给用户控制权
    - 依赖检查: shiny和miniUI不存在时优雅降级

[2026-05-17 00:14] === 编写 inst/rstudio/addins.dcf ===

  文件: D:/r-assistant/inst/rstudio/addins.dcf
  注册5个Addin:
    1. R Assistant Chat → addin_chat
    2. Explain Code → addin_explain
    3. Refactor Code → addin_refactor
    4. Fix Code → addin_fix
    5. Generate Docs → addin_document
  均标记为Interactive: true

[2026-05-17 00:15] === 编写 README.md ===

  文件: D:/r-assistant/README.md
  内容: 完整的中文使用文档
    - 功能列表表格
    - 支持的LLM提供商
    - 安装方法
    - 快速开始（配置API → 日常使用 → Addin使用）
    - 配置详解
    - 上下文感知说明
    - 对话历史管理
    - 项目结构树形图
    - 依赖包列表

================================================================================
第5章: 测试与验证
================================================================================

[2026-05-17 00:16] === 文件结构验证 ===

  总文件数: 12
  所有文件列表:
    D:/r-assistant/DESCRIPTION              (903 bytes)  — 包元数据
    D:/r-assistant/NAMESPACE                (1013 bytes) — 导出/导入声明
    D:/r-assistant/LICENSE                  (1070 bytes) — MIT许可证
    D:/r-assistant/README.md                (5335 bytes) — 中文使用文档
    D:/r-assistant/DEVELOPMENT_LOG.md       (开发日志)
    D:/r-assistant/R/api.R                  (3364 bytes) — API通信核心
    D:/r-assistant/R/config.R               (8491 bytes) — 配置管理
    D:/r-assistant/R/context.R              (5287 bytes) — 上下文采集
    D:/r-assistant/R/history.R              (2112 bytes) — 对话历史
    D:/r-assistant/R/assistant.R            (12490 bytes) — 核心业务逻辑
    D:/r-assistant/R/addin.R                (10901 bytes) — RStudio Addin
    D:/r-assistant/inst/rstudio/addins.dcf  (627 bytes)  — Addin注册

  R源码合计: 6个文件, ~42,645 bytes
  总文件: 12个

[2026-05-17 00:17] === 代码逻辑自检 ===

  ✓ DESCRIPTION: 包名、版本、依赖、编码声明完整
  ✓ NAMESPACE: 21个导出函数，5个importFrom声明
  ✓ api.R: PROVIDERS注册表包含5个提供商（openai/anthropic/deepseek/openrouter/custom）
            build_request_body处理Anthropic特殊格式（system字段单独提取）
            parse_response处理两种响应格式
  ✓ config.R: 配置读写、默认值合并、Shiny配置界面、环境变量key存储
  ✓ context.R: 7类上下文信息采集，均有tryCatch保护
  ✓ history.R: JSON持久化，自动裁剪，API格式转换
  ✓ assistant.R: 8个功能函数 + 2个内部辅助函数
                  .call_llm作为统一入口，带重试和超时
  ✓ addin.R: 5个Addin入口 + 2个UI辅助函数
             聊天气泡界面、代码提取、插入编辑器
  ✓ addins.dcf: 5个注册项，格式正确

  潜在注意事项:
  1. Anthropic provider的response解析路径(resp$content[[1]]$text)
     需要在实际API调用中验证
  2. addin_chat中的shinyjs::runjs需要用户安装shinyjs包
     （用于自动滚动到底部），缺失时不影响核心功能
  3. extract_code_blocks的正则在嵌套```场景可能有边界情况

================================================================================
第6章: 后续计划
================================================================================

  - 完成剩余R源文件 (context.R, assistant.R, history.R, addin.R)
  - 编写 addins.dcf
  - 编写 README.md
  - 编写测试用例
  - 用户安装和使用测试

================================================================================
日志结束（持续更新中）
================================================================================
