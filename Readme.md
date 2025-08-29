# Shiny Plot Annotator

Authors & Contributors: Federico Dallo (CNR-ISP), Claire Treat (Aarhus University)

An interactive Shiny app to visualize synchronized CO₂/CH₄ time series, add timestamped annotations by clicking on the plot, drag labels to refine timing (snapped to 5‑second grid), and export a clean, de‑duplicated annotations table.

> **Why?** Fast, auditable labeling of notable events (e.g., valve open/close) directly on normalized sensor signals.

---

## Features
- 📈 Plotly-based time series of **normalized** signals (0–1 range) for easy visual comparison:
  - `lgr_CO2_mean` (LGR CO₂)
  - `spl_conc_mean` (CO₂ proxy)
  - `lgr_CH4_mean` (LGR CH₄)
  - `lpl_conc_mean` (CH₄ proxy)
- 🖱️ **Click-to-annotate** with a quick chooser of common labels and an optional free‑text label.
- 🏷️ **Load existing annotations** and continue working; app merges and de‑duplicates by `(datetime, label)`.
- ↔️ **Draggable labels** (horizontally) to fine‑tune timing; movements snap to nearest **5 seconds**.
- 🔍 **Zoom/pan persistence** (Plotly `uirevision`) so you don’t lose your view while annotating.
- 💾 **CSV export** of merged annotations with `datetime` in UTC ISO‑8601.
- 🧱 Robust CSV readers and helpful validation messages for required columns and time parsing.

---

## Requirements
- R ≥ 4.1
- Packages: `shiny`, `plotly`, `dplyr`, `readr`, `lubridate`, `tools`
- Max upload size defaults to **200 MB** (set via `options(shiny.maxRequestSize = 200*1024^2)`).

### Install dependencies
```r
install.packages(c("shiny","plotly","dplyr","readr","lubridate","tools"))
```

(Optionally use `{renv}` for reproducibility.)

---

## Run the app
Save the provided script as `app.R`, then run:
```r
shiny::runApp(".")
```
By default the app opens in your browser at a local URL.

---

## Input data formats

### 1) **Joined CSV** (required)
- Upload a CSV containing **at least** the following columns:
  - `datetime` (UTC; ISO‑8601 like `YYYY-MM-DDTHH:MM:SSZ` **or** `YYYY-MM-DD HH:MM:SS`)
  - `lgr_CO2_mean`
  - `spl_conc_mean`
  - `lgr_CH4_mean`
  - `lpl_conc_mean`

Example (minimal):
```csv
datetime,lgr_CO2_mean,spl_conc_mean,lgr_CH4_mean,lpl_conc_mean
2025-07-01T10:00:00Z,411.2,0.482,1.874,0.051
2025-07-01T10:00:05Z,412.0,0.487,1.881,0.052
2025-07-01T10:00:10Z,413.5,0.495,1.905,0.053
```

**Time parsing:**
- If `datetime` is not already POSIXct, the app tries `lubridate::ymd_hms()` (UTC) and then `readr::parse_datetime()` (UTC).
- On failure you’ll get a clear validation message.

**Normalization:**
- Each signal is normalized 0–1 using `norm01(x) = (x - min) / (max - min)` (per full dataset; NAs allowed).

### 2) **Annotations CSV** (optional)
- You can upload an existing annotations file to continue labeling.
- Required columns:
  - `datetime` (UTC, parseable to POSIXct)
  - `label` (string)

Example:
```csv
datetime,label
2025-07-01T10:03:20Z,COM3-OPEN
2025-07-01T10:17:05Z,COM3-CLOSE
```

**Merging rules:** Uploaded annotations are combined with current session annotations and **de‑duplicated** by `(datetime, label)` then sorted by time.

---

## Annotation workflow
1. **Upload** your joined CSV.
2. Inspect the normalized traces; **click** on the plot to open the *Add Annotation* dialog.
3. **Choose** a label from the dropdown *(default choices: `COM3-OPEN`, `COM3-CLOSE`)* or **type** a new label.
4. The annotation appears as:
   - a **vertical dotted marker** at the timestamp; and
   - a **rotated draggable label** on the top (drag horizontally to adjust).
5. Drags are snapped to the **nearest 5 seconds** and written back to the table.
6. (Optional) **Upload** an annotations CSV to continue where you left off. New labels found in the file are added to the chooser.
7. **Download** your merged annotations via *Download annotations (CSV)*.

**Tip:** The app remembers your current x‑axis zoom/pan until you explicitly autorange.

---

## Output format
The download handler produces a CSV named like:
```
<uploaded_base>_annotations_<YYYYMMDD>_<HHMMSS>.csv
```
with columns:
- `datetime` — UTC in ISO‑8601 (`YYYY-MM-DDTHH:MM:SSZ`)
- `label` — string

If no annotations exist, an empty table with these headers is exported.

---

## UI overview
- **Left panel**: data upload (joined CSV), annotations upload (optional), help text, and download button.
- **Main panel**: Plotly chart (480px height) + live readout of the **sorted, de‑duplicated** annotations table.

**Series legend & colors:**
- LGR CO₂ — red
- SPL CO₂ proxy — orange
- LGR CH₄ — blue
- LPL CH₄ proxy — green

---

## Validation & edge cases
- Missing required columns → informative validation message listing what’s missing.
- Unparseable `datetime` → message prompting ISO‑8601 or `YYYY-MM-DD HH:MM:SS` in UTC.
- Constant series (no variance) in normalization → returns `NA` for that series.
- Duplicate annotations → automatically removed in the displayed table and in the download.

---

## Known limitations
- Label drag only affects the **x** coordinate; vertical moves are ignored and labels reset to the top band.
- Snap grid is fixed at **5 seconds**.
- All times are treated as **UTC**.

---

## Troubleshooting
- **Nothing plots / empty x‑axis**: check that `datetime` parses (use ISO‑8601 with `Z`).
- **Validation error about columns**: ensure your joined CSV has exactly the required headers.
- **Labels won’t stay at new time**: ensure you’re dragging **horizontally**; vertical drags are ignored.
- **Export shows no rows**: you haven’t added or loaded any annotations yet.

---

## Development notes (mapping to code)
- `norm01()` implements min–max scaling with NA tolerance and degenerate-range guard.
- `to_posix_from_plotly()` converts Plotly x values (ms vs s epoch or ISO strings) to `POSIXct` UTC.
- `read_annotations_csv()` enforces schema, parses UTC timestamps, de‑duplicates, and sorts.
- `rv$label_choices` seeds with `COM3-OPEN`, `COM3-CLOSE`; new free‑text labels are appended.
- Plot uses `uirevision = "keep"`; vertical markers are non‑draggable **segments**; labels are **annotations**.
- `plotly_relayout` handler:
  - stores current x‑range (unless autoranged),
  - captures `annotations[i].x` updates,
  - snaps to 5s via `round_to_5s()`,
  - updates the underlying `rv$annotations` (with de‑dup & sort).
- Download writes UTC timestamps in ISO‑8601 (`%Y-%m-%dT%H:%M:%SZ`).

---

## Folder layout
```
project/
├─ app.R                 # the provided Shiny app
├─ data/                 # (optional) input CSVs
├─ src/                  # (optional) additional source code
└─ annotations/          # (optional) saved annotation CSVs
```

---

## License
GNU General Public License (GPL)

---

## Citation / Attribution
If you use this tool in a publication, please cite this project/report and this app as: *“Shiny Plot Annotator (version 2025‑08), ParticularMatter.”*

The work was supported by European Union under MISO project, “Autonomous Multi-Format In-Situ Observation Platform for Atmospheric Carbon Dioxide and Methane Monitoring in Permafrost & Wetlands”  (Grant ID: 101086541)

---

## Changelog
- **v1.0.0** — Initial release with upload, annotate, drag‑to‑adjust (5s snap), merge & export.

