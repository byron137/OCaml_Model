# 結論（改進一、二）— 依 spec §5.1「一頁結論」

完整表格在 `REPORT.md`，per-slice 資料在 `results/<exp>/per_slice.csv`，2× 裁切與熱圖在 `results/crops/`。
條件：2-slice（360 px）、target 24 blocks = 6.4 bpp（3.75x）、table_num 4、RLC on、palette off、
合成測試集 5 類 × 6 張（720×96，`images/test`）、E2.2 / E2.4 的 codebook 用 `images/train` 離線建立。
Bits 是精確的 dyadic rANS payload（每符號 km − ks），不是 codeword 長度代理。

## 先講三件與 spec 前提不同、會影響解讀的事

1. **現行 model 的 DPCM 是全串列鏈（pred(4k) = recon(4k−1)），不是 spec §0.1 寫的 stride-4 主鏈。**
   依你的決定，基線用串列鏈（E2.0），stride-4 主鏈另做成 `Trunk4` 變體（E2.0_trunk4）。
   實測 Trunk4 比串列鏈 **多 7.3 % bits、PSNR −0.57 dB**，而變體 A、B 都必須建立在 stride-4 主鏈上。
2. **測試集是合成的**，照片類是程序生成（noise + blur），不代表真實照片；文字類有 ClearType 彩邊。
3. 保底 macro（idx 12）的 data_num 是固定的 6 bpp，**沒有空間放 M1c 的 side info**：L = 20 時 fallback slice
   會超出 target 1 個 block（REPORT 的 over target 欄），L = 36 才剛好塞得下。

## 改進一：Macro mode 的 chroma 奇數像素重建

| | Δbits | ΔPSNR（全類別） | ΔE00 p99（UI / 文字） | §1.4 準則 |
|---|---|---|---|---|
| E1.1 M1b vs E1.0 M1a | 0 | +0.55 dB | 17.8→19.5（+10 %）/ 36.2→34.0（−6 %） | PSNR：4 類通過，**UI −0.35 dB 未過**；chroma max err：照片 33→17、漸層 10→7，有硬彩邊的類別仍 255 |
| E1.2 M1c (L=20) vs E1.1 | +0.30 bpp side info | +1.09 dB | 19.5→13.5（**−30.8 %**）/ 34.0→29.4（**−13.4 %**） | UI 通過、**文字未達 −20 %**、照片 +0.06 dB 通過 → 整體 NOT MET |
| E1.3 L=12 / L=36 vs L=20 | +0.20 / −0.13 bpp | +0.11 / −0.17 dB | UI 12.9 / 14.8 | L=36 不會超 target；L=12 增益小於其 side info 成本 |
| E1.4 正常 arbiter（只看真的落入 macro 的 1188 個 slice，全在文字與雜訊類） | M1c +108 bits/slice | M1a 20.26 → M1b 20.80 → M1c 21.29 dB | — | 實際情境下 M1b 零成本 +0.5 dB，M1c 再 +0.5 dB |

- M1b 的「色暈」如 spec 預期出現在 UI（ΔE00 p99 變差），M1c 的 β 把它消掉並超過基線；照片三者差異很小。
- β 直方圖：idx 0（β = 0）佔 64–71 %，其餘集中在 +½、+1、−1；**沒有出現新的色斑**（照片 max err 17→18）。
- 文字類未達標的原因如 spec §1.3 所述：ClearType 彩邊的 luma-chroma 關係不是單一色相；β 強制為 0 的 segment 比例見上表（≈ 70 %）。
- **建議**：M1b 零成本、全類別 +0.55 dB，直接保留；M1c 的去留依 spec 交由第三步 ladder 統一評估，若採用需同時讓保底 data_num 容納 side info，或改用 L = 36。

## 改進二：Full 模式改 interpolative 預測

| | Δbits vs 串列（E2.0） | Δbits vs Trunk4 | ΔPSNR vs E2.0 | §2.6 準則 |
|---|---|---|---|---|
| E2.0_trunk4（spec 假設的現況） | **+7.29 %** | 0 | −0.57 dB | — |
| E2.1 變體 A，共用 codebook | **+7.38 %** | **+0.08 %** | −0.58 dB | NOT MET（預期 −3 ~ −5 %） |
| E2.2 A + lane 3 獨立 codebook | +6.99 % | −0.36 % vs E2.1 | −0.74 dB | 增益 < 0.4 % |
| E2.3 變體 B，共用 codebook | +8.69 % | +1.22 % vs E2.1 | −0.90 dB | bits 在 ±1.5 % 內、lane 1/3 零與 ±1 占比 chroma 96 %、Y 77 % → **形式上 PASS**，但絕對值比基線差 |
| E2.4 B + 奇偶 codebook | +3.06 % | −5.17 % vs E2.3 | −1.11 dB | 增益主要來自 codebook：串列鏈 + 同樣的奇偶 codebook（E2.4_serial_oddeven）就是 **−4.06 %** |

- **變體 A 的插值增益本身存在但太小**：照片類 lane 3 的殘差熵 1.90 → 1.74 bits（−8 %），但 stride-4 主鏈讓 lane 0 的熵 1.51 → 2.01（+33 %），把增益吃掉好幾倍；漸層類 lane 3 完全沒有改善（0.80 → 0.83），因為主鏈 lane 0 用的是較粗的 anchor 量化表，插值的兩個端點比串列鏈的鄰居更吵。
- E2.5 分類別：A 在 UI +38.8 %、漸層 +22.6 %（硬邊與平緩斜坡上，串列鏈預測幾乎是零殘差，插值反而在邊的兩側各留一半）；照片 −0.5 %、文字 +0.8 %。B 比 A 在 UI 再 +7.2 %、漸層 +3.3 %、文字 +1.0 %、照片 −0.9 %；spec 擔心的「文字類 lane 2 stride-2 代價」存在但不是最差的類別，UI 才是。
- E2.6 附錄：1-slice 與 4-slice 結果方向一致（A +8 ~ +6 %，B +9 ~ +8 %）。
- 奇偶 / lane 3 codebook 與 RTL arbiter 常數（zero_cost = 12、escape_cost）不再一致，E2.4 有 6 % 的 slice 實際超出 target；若採用訓練 codebook，arbiter 常數要一起重算。

**依 spec §5.2 步驟 2 的停止條件（Δbits > −1 % → 回報失效類別再決定）：改進二在本 model 上失效，失效類別為全部，根本原因是 spec 的 AR(1) 推導建立在 stride-4 主鏈的基線上，而現行 model 是串列鏈，串列鏈已經拿走了 spec 想回收的那份資訊。** 改進三（Drop / ladder）建立在變體 B 的奇偶結構上，是否繼續、或改成在串列鏈上另行設計 Drop，需要你決定。

## 可直接採用的零成本項目

1. M1b（+0.55 dB，bitstream 不變）。
2. 離線訓練的 codebook（奇偶或單一）在串列鏈上 −4.1 % bits，PSNR −0.26 dB（主要是 arbiter 常數失配造成的 over target，修常數後應可收回）。

## 交付物對照（§5.1）

- Model 原始碼：`codec_gm/lib/{interp,xcodec,xrdo,xbook}.ml`、`bin/{exp,build_books}.ml`；QCheck 性質測試 `test/test_xcodec.ml`：(a) encode→decode 重建值 = encoder 內部重建值（每種 structure × M1）、Serial/M1a 與 golden model 的重建值與 bits 完全相等、Xrdo = Rdo；(c) 偶數重建值不受 M1 影響；(b) Drop 段的 max err ≤ T 屬改進三，尚未實作。
- 結果表與 per-slice CSV：`results/REPORT.md`、`results/<exp>/`。
- 視效包：`results/crops/{E1,E2A,E2B}_<category>.png`（原圖 / 基線 / 實驗 / 兩張 0..32 熱圖，2×）。
- Ladder 表：屬改進三，未做。
