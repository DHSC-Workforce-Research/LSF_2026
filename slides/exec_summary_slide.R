# ===========================================================================
# exec_summary_slide.R
# Renders the LSF executive summary as a 16:9 slide PNG (DHSC / GSS style).
# Dependencies: ggplot2 + stringr only. Save the PNG, drop it into PowerPoint.
#   source("exec_summary_slide.R")  ->  writes exec_summary.png
# ===========================================================================
suppressMessages({ library(ggplot2); library(stringr) })

GBP <- "£"   # pound sign, built by escape so the source stays ASCII-safe

# ---- content ---------------------------------------------------------------
ideas <- list(
  list(n="1", col="#12436D",
       head="A first: we can follow funded students across their whole course",
       bul=c(
         paste0("New BSA data share: a de facto census of ~290,000 students, 2020 to 2026"),
         "Each student followed from entry to last claim; demographics added")),
  list(n="2", col="#28A197",
       head="The grant's real value has fallen and varies widely",
       bul=c(
         paste0("Down ~23% since 2020; the ", GBP, "5,000 is not indexed"),
         paste0("Worth full value in the cheapest areas, ~", GBP, "1,500 in central London; top-ups widen the range"))),
  list(n="3", col="#801650",
       head="More money is linked to staying, but the effect is small and not causal",
       bul=c(
         paste0(GBP, "1,000 less real grant: 6 to 8% more likely to leave, now and next year. Robust to controls"),
         "Dependence on the grant predicts leaving; awareness does not",
         "Everyone is funded, so no proof of cause. No model predicts who leaves (scores 56%, near chance)",
         "No link between an area's grant value and how many students it recruits")),
  list(n="4", col="#F46A25",
       head="Leaving is steady and broad, and some groups feel the strain more",
       bul=c(
         "A third leave before finishing, ~18% a year, steady and spread over years one and two, not a first-year cliff",
         "Parents stay (~12% less likely); placement hours do not track leaving",
         "Worry and dependence concentrate in older, disabled and some minority ethnic groups",
         "Sharpest signal is a student's own 'I may have to leave': 22% go the next year, against 15%"))
)
scope <- paste0("Out of scope: recruitment volume, entry into the NHS, and value for money. ",
                "The grant reaches precarious students; the data does not say which component to re-target.")

ink <- "#0B0C0C"; grey <- "#6F777B"

# ---- layout engine ---------------------------------------------------------
# two flowing columns on a 0-100 canvas. Left = ideas 1,3; right = ideas 2,4
# (six bullets each, so the columns balance). Title band + scope footer.
columns <- list(
  list(x0=2,  x1=49, ideas=c(1,3)),
  list(x0=51, x1=98, ideas=c(2,4))
)

rects <- data.frame(); pts <- data.frame(); txt <- data.frame()
WRAP   <- 54          # wrap width in characters for bullets
LH     <- 2.6         # y-units per wrapped text line
HEAD_H <- 6.6         # header bar height
TOP    <- 87.5        # top of the content area
IDEA_GAP <- 3.0       # gap between stacked ideas
BUL_GAP  <- 1.9       # gap between bullets
FOOT_TOP <- 12.5      # bottom content must clear this; footer sits below

ROW2 <- 55            # fixed top for the second idea in each column, so headers align

for (col in columns) {
  y <- TOP
  ri <- 0
  for (k in col$ideas) {
    ri <- ri + 1
    if (ri == 2) y <- ROW2            # align the lower headers across columns
    a <- ideas[[k]]
    # header bar
    rects <- rbind(rects, data.frame(xmin=col$x0, xmax=col$x1, ymin=y-HEAD_H, ymax=y, fill=a$col))
    hd <- str_wrap(paste0(a$n, ".  ", a$head), width = 44)
    txt <- rbind(txt, data.frame(x=col$x0+1.4, y=y-HEAD_H/2, label=hd, col="white",
                                 size=5.0, face="bold", hjust=0, vjust=0.5, family="Arial"))
    y <- y - HEAD_H - 3.0
    # bullets
    for (b in a$bul) {
      w  <- str_wrap(b, width = WRAP)
      nl <- length(strsplit(w, "\n")[[1]])
      pts <- rbind(pts, data.frame(x=col$x0+1.8, y=y-0.7, col=a$col))
      txt <- rbind(txt, data.frame(x=col$x0+3.6, y=y, label=w, col=ink,
                                   size=4.2, face="plain", hjust=0, vjust=1, family="Arial"))
      y <- y - nl*LH - BUL_GAP
    }
    y <- y - IDEA_GAP
  }
}

# scope footer: padded grey box with a dark-blue accent bar, text centred
rects <- rbind(rects, data.frame(xmin=2, xmax=98, ymin=1.5, ymax=10.5, fill="#F2F2F2"))
rects <- rbind(rects, data.frame(xmin=2, xmax=2.7, ymin=1.5, ymax=10.5, fill="#12436D"))
txt   <- rbind(txt, data.frame(x=4.2, y=6, label=str_wrap(scope, width=150), col=grey,
                               size=3.4, face="plain", hjust=0, vjust=0.5, family="Arial"))

# ---- draw ------------------------------------------------------------------
p <- ggplot() +
  # title band
  annotate("rect", xmin=0, xmax=100, ymin=90.5, ymax=100, fill="#12436D") +
  annotate("text", x=2, y=95.2, label="Executive summary", hjust=0, vjust=0.5,
           colour="white", fontface="bold", size=8, family="Arial") +
  annotate("text", x=98, y=95.2, label="LSF longitudinal analysis  |  first look",
           hjust=1, vjust=0.5, colour="#B8D0E6", size=3.6, family="Arial") +
  geom_rect(data=rects, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=I(fill))) +
  geom_point(data=pts, aes(x=x, y=y, colour=I(col)), size=1.5) +
  geom_text(data=txt, aes(x=x, y=y, label=label, colour=I(col), size=I(size),
                          fontface=face, hjust=hjust, vjust=vjust, family=family),
            lineheight=0.95) +
  scale_x_continuous(limits=c(0,100), expand=c(0,0)) +
  scale_y_continuous(limits=c(0,100), expand=c(0,0)) +
  theme_void() + theme(plot.margin=margin(0,0,0,0))

ggsave("exec_summary.png", p, width=13.33, height=7.5, units="in", dpi=200, bg="white")
cat("wrote exec_summary.png\n")
