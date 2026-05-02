"""
Tornado Map Configurator
By: Jon Sammons
========================
A GUI tool for configuring and generating a customized version of
conus_tornado_map_v5_0.R without manually editing the R script. Based
on your inputs and the file paths you provide, it updates the required
values and writes a ready-to-run copy of the script.

BLUF: Save yourself time from manually editing the R script.

Requirements: Python 3.x (tkinter is included in the standard library)

Usage:
    python tornado_map_configurator.py
"""

import tkinter as tk
from tkinter import ttk, filedialog, messagebox
import re
import os


import datetime

def get_all_decades():
    """
    For those of you in the future, the list of decades is built dynamically so the GUI stays current without
    code changes. It only includes decades that have fully elapsed — for example, in
    2026 the 2020s are excluded because 2029 has not yet passed. The 2020s
    will appear automatically once the year reaches 2030.
    """
    current_year = datetime.date.today().year
    decades = []
    d = 1950
    while d + 9 < current_year:   # decade must be fully complete. See my note above.
        decades.append(f"{d}s")
        d += 10
    return decades

ALL_DECADES = get_all_decades()

# Census column names follow the NHGIS A00AA{year} convention.
# If NHGIS changes this naming in a future release, update the values here. Dear NHGIS, please don’t.
DEFAULT_DECADE_CONFIG = {
    dec: f"A00AA{dec[:4]}" for dec in ALL_DECADES
}


# ── Helper: build the R list literal for decade_config ───────────────────────
def build_decade_config_r(decade_col_map):
    """Return the R list(...) string for decade_config."""
    lines = ["list("]
    items = list(decade_col_map.items())
    for i, (dec, col) in enumerate(items):
        comma = "," if i < len(items) - 1 else ""
        lines.append(f'  "{dec}" = "{col}"{comma}')
    lines.append(")")
    return "\n".join(lines)


# ── Helper: build the R c(...) string for study_decades ──────────────────────
def build_study_decades_r(selected):
    quoted = ', '.join(f'"{d}"' for d in selected)
    return f"c({quoted})"


# ── Main application window ───────────────────────────────────────────────────
class TornadoMapConfigurator(tk.Tk):
    def __init__(self):
        super().__init__()
        self.title("Tornado Map Configurator — v5.0")
        self.resizable(True, True)
        self.minsize(700, 600)

        # Track decade checkbox states and census column entries
        self.decade_vars = {}       # decade -> BooleanVar (checked = included)
        self.census_entries = {}    # decade -> StringVar (census column name)

        self._build_ui()
        self._set_defaults()

    # ── UI construction ───────────────────────────────────────────────────────
    def _build_ui(self):
        # Scrollable main frame
        canvas = tk.Canvas(self, borderwidth=0)
        scrollbar = ttk.Scrollbar(self, orient="vertical", command=canvas.yview)
        self.scroll_frame = ttk.Frame(canvas)

        self.scroll_frame.bind(
            "<Configure>",
            lambda e: canvas.configure(scrollregion=canvas.bbox("all"))
        )
        canvas.create_window((0, 0), window=self.scroll_frame, anchor="nw")
        canvas.configure(yscrollcommand=scrollbar.set)

        canvas.pack(side="left", fill="both", expand=True)
        scrollbar.pack(side="right", fill="y")

        # Bind mousewheel scrolling
        canvas.bind_all("<MouseWheel>",
                        lambda e: canvas.yview_scroll(int(-1*(e.delta/120)), "units"))

        pad = {"padx": 10, "pady": 4}
        f = self.scroll_frame

        row = 0

        # ── Title ─────────────────────────────────────────────────────────────
        ttk.Label(f, text="Tornado Map Configurator",
                  font=("Helvetica", 14, "bold")).grid(
            row=row, column=0, columnspan=3, pady=(12, 2), padx=10, sticky="w")
        row += 1
        ttk.Label(f, text="Configure and generate a customized conus_tornado_map_v5_0.R",
                  foreground="gray").grid(
            row=row, column=0, columnspan=3, padx=10, sticky="w")
        row += 1
        ttk.Separator(f, orient="horizontal").grid(
            row=row, column=0, columnspan=3, sticky="ew", padx=10, pady=6)
        row += 1

        # ── Section 1: Template script ────────────────────────────────────────
        ttk.Label(f, text="1. Template R Script", font=("Helvetica", 11, "bold")).grid(
            row=row, column=0, columnspan=3, sticky="w", **pad)
        row += 1

        ttk.Label(f, text="Template R script:").grid(row=row, column=0, sticky="e", **pad)
        self.template_var = tk.StringVar()
        ttk.Entry(f, textvariable=self.template_var, width=55).grid(
            row=row, column=1, sticky="ew", **pad)
        ttk.Button(f, text="Browse…",
                   command=self._browse_template).grid(row=row, column=2, **pad)
        row += 1

        ttk.Label(f, text="Output R script:").grid(row=row, column=0, sticky="e", **pad)
        self.output_script_var = tk.StringVar()
        ttk.Entry(f, textvariable=self.output_script_var, width=55).grid(
            row=row, column=1, sticky="ew", **pad)
        ttk.Button(f, text="Browse…",
                   command=self._browse_output_script).grid(row=row, column=2, **pad)
        row += 1

        ttk.Separator(f, orient="horizontal").grid(
            row=row, column=0, columnspan=3, sticky="ew", padx=10, pady=6)
        row += 1

        # ── Section 2: Decade configuration ───────────────────────────────────
        ttk.Label(f, text="2. Decade Configuration",
                  font=("Helvetica", 11, "bold")).grid(
            row=row, column=0, columnspan=3, sticky="w", **pad)
        row += 1
        ttk.Label(f,
                  text="Check the decades to include. Edit census column names if needed.",
                  foreground="gray").grid(
            row=row, column=0, columnspan=3, sticky="w", padx=10)
        row += 1

        # Column headers
        ttk.Label(f, text="Include", font=("Helvetica", 9, "bold")).grid(
            row=row, column=0, sticky="w", padx=(10, 0))
        ttk.Label(f, text="Decade", font=("Helvetica", 9, "bold")).grid(
            row=row, column=1, sticky="w", padx=(0, 0))
        ttk.Label(f, text="NHGIS Census Column", font=("Helvetica", 9, "bold")).grid(
            row=row, column=2, sticky="w")
        row += 1

        for dec in ALL_DECADES:
            var = tk.BooleanVar(value=False)
            col_var = tk.StringVar(value=DEFAULT_DECADE_CONFIG[dec])
            self.decade_vars[dec] = var
            self.census_entries[dec] = col_var

            cb = ttk.Checkbutton(f, variable=var,
                                 command=self._on_decade_toggle)
            cb.grid(row=row, column=0, sticky="w", padx=(20, 0), pady=2)

            ttk.Label(f, text=dec, width=8).grid(
                row=row, column=1, sticky="w", pady=2)

            entry = ttk.Entry(f, textvariable=col_var, width=18)
            entry.grid(row=row, column=2, sticky="w", pady=2, padx=(0, 10))

            row += 1

        ttk.Separator(f, orient="horizontal").grid(
            row=row, column=0, columnspan=3, sticky="ew", padx=10, pady=6)
        row += 1

        # ── Section 3: Year range ──────────────────────────────────────────────
        ttk.Label(f, text="3. Tornado Data Year Range",
                  font=("Helvetica", 11, "bold")).grid(
            row=row, column=0, columnspan=3, sticky="w", **pad)
        row += 1
        ttk.Label(f, text="Must span all selected decades.",
                  foreground="gray").grid(
            row=row, column=0, columnspan=3, sticky="w", padx=10)
        row += 1

        ttk.Label(f, text="Year min:").grid(row=row, column=0, sticky="e", **pad)
        self.year_min_var = tk.StringVar(value="1980")
        ttk.Entry(f, textvariable=self.year_min_var, width=10).grid(
            row=row, column=1, sticky="w", **pad)
        row += 1

        ttk.Label(f, text="Year max:").grid(row=row, column=0, sticky="e", **pad)
        self.year_max_var = tk.StringVar(value="2019")
        ttk.Entry(f, textvariable=self.year_max_var, width=10).grid(
            row=row, column=1, sticky="w", **pad)
        row += 1

        ttk.Separator(f, orient="horizontal").grid(
            row=row, column=0, columnspan=3, sticky="ew", padx=10, pady=6)
        row += 1

        # ── Section 4: Classification threshold ───────────────────────────────
        ttk.Label(f, text="4. Hotspot Classification Threshold",
                  font=("Helvetica", 11, "bold")).grid(
            row=row, column=0, columnspan=3, sticky="w", **pad)
        row += 1
        ttk.Label(f,
                  text="Fraction of decades a county must be significant to be 'Persistent'.\n"
                       "Default 0.75 = 3 of 4 decades (matches original study).",
                  foreground="gray", justify="left").grid(
            row=row, column=0, columnspan=3, sticky="w", padx=10)
        row += 1

        ttk.Label(f, text="Persist fraction:").grid(row=row, column=0, sticky="e", **pad)
        self.persist_var = tk.StringVar(value="0.75")
        ttk.Entry(f, textvariable=self.persist_var, width=10).grid(
            row=row, column=1, sticky="w", **pad)
        row += 1

        ttk.Separator(f, orient="horizontal").grid(
            row=row, column=0, columnspan=3, sticky="ew", padx=10, pady=6)
        row += 1

        # ── Section 5: Data file paths ─────────────────────────────────────────
        ttk.Label(f, text="5. Data File Paths",
                  font=("Helvetica", 11, "bold")).grid(
            row=row, column=0, columnspan=3, sticky="w", **pad)
        row += 1

        file_fields = [
            ("County shapefile (.shp):", "shp_var",
             [("Shapefiles", "*.shp"), ("All files", "*.*")]),
            ("Population CSV (NHGIS):", "pop_var",
             [("CSV files", "*.csv"), ("All files", "*.*")]),
            ("Tornado CSV (NOAA SPC):", "tornado_var",
             [("CSV files", "*.csv"), ("All files", "*.*")]),
            ("Output HTML map:", "html_var",
             [("HTML files", "*.html"), ("All files", "*.*")]),
        ]

        for label, attr, ftypes in file_fields:
            setattr(self, attr, tk.StringVar())
            ttk.Label(f, text=label).grid(row=row, column=0, sticky="e", **pad)
            ttk.Entry(f, textvariable=getattr(self, attr), width=55).grid(
                row=row, column=1, sticky="ew", **pad)
            # Capture ftypes in closure
            ttk.Button(f, text="Browse…",
                       command=lambda ft=ftypes, a=attr: self._browse_file(a, ft)).grid(
                row=row, column=2, **pad)
            row += 1

        ttk.Separator(f, orient="horizontal").grid(
            row=row, column=0, columnspan=3, sticky="ew", padx=10, pady=6)
        row += 1

        # ── Generate button + status ───────────────────────────────────────────
        btn_frame = ttk.Frame(f)
        btn_frame.grid(row=row, column=0, columnspan=3, pady=10)

        ttk.Button(btn_frame, text="Generate R Script",
                   command=self._generate).pack(side="left", padx=6)
        ttk.Button(btn_frame, text="Reset to Defaults",
                   command=self._set_defaults).pack(side="left", padx=6)

        row += 1
        self.status_var = tk.StringVar(value="")
        self.status_label = ttk.Label(f, textvariable=self.status_var,
                                      foreground="green", wraplength=600,
                                      justify="left")
        self.status_label.grid(row=row, column=0, columnspan=3,
                               padx=10, pady=(0, 12), sticky="w")

        # Make column 1 expandable
        f.columnconfigure(1, weight=1)

    # ── Default values ────────────────────────────────────────────────────────
    def _set_defaults(self):
        """Reset all controls to the original study configuration."""
        for dec in ALL_DECADES:
            self.census_entries[dec].set(DEFAULT_DECADE_CONFIG[dec])

        # Default study: 1980s–2010s checked
        for dec in ALL_DECADES:
            self.decade_vars[dec].set(dec in ("1980s", "1990s", "2000s", "2010s"))

        self.year_min_var.set("1980")
        self.year_max_var.set("2019")
        self.persist_var.set("0.75")
        self.shp_var.set("")
        self.pop_var.set("")
        self.tornado_var.set("")
        self.html_var.set("C:/your/output/folder/conus_county_tornado_map_v5.html")
        self.status_var.set("")

    # ── Browse helpers ────────────────────────────────────────────────────────
    def _browse_template(self):
        path = filedialog.askopenfilename(
            title="Select template R script",
            filetypes=[("R scripts", "*.R"), ("All files", "*.*")])
        if path:
            self.template_var.set(path)

    def _browse_output_script(self):
        path = filedialog.asksaveasfilename(
            title="Save generated R script as",
            defaultextension=".R",
            filetypes=[("R scripts", "*.R"), ("All files", "*.*")])
        if path:
            self.output_script_var.set(path)

    def _browse_file(self, attr, filetypes):
        path = filedialog.askopenfilename(title="Select file", filetypes=filetypes)
        if path:
            getattr(self, attr).set(path)

    def _on_decade_toggle(self):
        """Auto-update year_min/year_max when decades are toggled."""
        selected = [d for d in ALL_DECADES if self.decade_vars[d].get()]
        if selected:
            earliest = int(selected[0][:4])
            latest   = int(selected[-1][:4]) + 9
            self.year_min_var.set(str(earliest))
            self.year_max_var.set(str(latest))

    # ── Validation ────────────────────────────────────────────────────────────
    def _validate(self):
        errors = []

        if not self.template_var.get():
            errors.append("Please select the template R script.")
        elif not os.path.isfile(self.template_var.get()):
            errors.append(f"Template R script not found:\n  {self.template_var.get()}")

        if not self.output_script_var.get():
            errors.append("Please specify an output R script path.")

        selected = [d for d in ALL_DECADES if self.decade_vars[d].get()]
        if len(selected) < 2:
            errors.append("Select at least 2 decades.")

        try:
            ymin = int(self.year_min_var.get())
            ymax = int(self.year_max_var.get())
            if ymin >= ymax:
                errors.append("Year min must be less than year max.")
        except ValueError:
            errors.append("Year min and max must be integers.")

        try:
            pf = float(self.persist_var.get())
            if not (0 < pf <= 1):
                errors.append("Persist fraction must be between 0 and 1.")
        except ValueError:
            errors.append("Persist fraction must be a number (e.g. 0.75).")

        return errors

    # ── Script generation ─────────────────────────────────────────────────────
    def _generate(self):
        self.status_var.set("")
        self.status_label.configure(foreground="red")

        errors = self._validate()
        if errors:
            messagebox.showerror("Validation errors", "\n\n".join(errors))
            return

        # Read template
        try:
            with open(self.template_var.get(), "r", encoding="utf-8") as fh:
                script = fh.read()
        except Exception as e:
            messagebox.showerror("Error", f"Could not read template:\n{e}")
            return

        selected   = [d for d in ALL_DECADES if self.decade_vars[d].get()]
        dec_config = {d: self.census_entries[d].get() for d in selected}

        # ── 1. Replace decade_config list ────────────────────────────────────
        new_dc = build_decade_config_r(dec_config)
        script = re.sub(
            r'decade_config\s*<-\s*list\([^)]*\)',
            f'decade_config <- {new_dc}',
            script,
            flags=re.DOTALL
        )

        # ── 2. Replace study_decades ──────────────────────────────────────────
        new_sd = build_study_decades_r(selected)
        script = re.sub(
            r'study_decades\s*<-\s*c\([^)]*\)',
            f'study_decades <- {new_sd}',
            script
        )

        # ── 3. Replace year range ─────────────────────────────────────────────
        script = re.sub(
            r'tornado_year_min\s*<-\s*\d+',
            f'tornado_year_min <- {self.year_min_var.get()}',
            script
        )
        script = re.sub(
            r'tornado_year_max\s*<-\s*\d+',
            f'tornado_year_max <- {self.year_max_var.get()}',
            script
        )

        # ── 4. Replace persist_fraction ───────────────────────────────────────
        script = re.sub(
            r'persist_fraction\s*<-\s*[\d.]+',
            f'persist_fraction <- {self.persist_var.get()}',
            script
        )

        # ── 5. Replace file paths (only if user provided them) ───────────────
        def replace_path(text, var_name, new_path):
            if not new_path:
                return text
            # Match the assignment line, preserve the rest of the line (comments)
            return re.sub(
                rf'({var_name}\s*<-\s*")[^"]*(")',
                rf'\g<1>{new_path.replace(chr(92), "/")}\g<2>',
                text
            )

        script = replace_path(script, "county_shp",  self.shp_var.get())
        script = replace_path(script, "pop_csv",     self.pop_var.get())
        script = replace_path(script, "tornado_csv", self.tornado_var.get())
        script = replace_path(script, "out_html",    self.html_var.get())

        # ── Write output ──────────────────────────────────────────────────────
        out_path = self.output_script_var.get()
        try:
            os.makedirs(os.path.dirname(out_path) or ".", exist_ok=True)
            with open(out_path, "w", encoding="utf-8") as fh:
                fh.write(script)
        except Exception as e:
            messagebox.showerror("Error", f"Could not write output script:\n{e}")
            return

        # ── Success message ───────────────────────────────────────────────────
        n_dec  = len(selected)
        n_early = n_dec // 2
        n_late  = n_dec - n_early
        persist = int(float(self.persist_var.get()) * n_dec + 0.9999)  # ceiling

        summary = (
            f"✓  Script written to:\n   {out_path}\n\n"
            f"   Decades ({n_dec}):  {', '.join(selected)}\n"
            f"   Early / Late:     {', '.join(selected[:n_early])}  /  "
            f"{', '.join(selected[n_early:])}\n"
            f"   Year range:       {self.year_min_var.get()} – {self.year_max_var.get()}\n"
            f"   Persist threshold: {persist} of {n_dec} decades "
            f"({self.persist_var.get()} fraction)"
        )
        self.status_label.configure(foreground="green")
        self.status_var.set(summary)


# ── Entry point ───────────────────────────────────────────────────────────────
if __name__ == "__main__":
    app = TornadoMapConfigurator()
    app.mainloop()
