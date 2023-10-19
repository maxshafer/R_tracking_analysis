
## Instructions for how to add light pulses and different shading to multi-day graphs (week long or longer)

### This should work for any data that is multi-day (7 day weeks, or 14 days, or whatever)
### Make the data as usual, and plot the data as usual
### Use 'geom_rect_shading_bz_Ndays', instead of 'geom_rect_shading_bz_7days

### For experiments that are longer or shorter, count the number of days, including both the setup day and the day the experiment ends
### Because week-long data includes information from Wed-Tues + Wed, there are actually 8 days from which data can come from, so set 'n_days = 8'

### To add shading for different periods, use the new argument 'date_time_shade'
### This argument takes a dataframe in the form of the following:

dts <- data.frame(xstart = c("1970-01-04 02:30:00", "1970-01-05 02:30:00", "1970-01-02 10:30:00", "1970-01-06 00:00:00"), xend = c("1970-01-04 03:30:00", "1970-01-05 03:30:00", "1970-01-02 14:30:00", "1970-01-07 23:59:00"), col = c("light_pulse", "light_pulse", "injection", "dark_dark"))
dts

# xstart                xend         col
# 1 1970-01-04 02:30:00 1970-01-04 03:30:00 light_pulse
# 2 1970-01-05 02:30:00 1970-01-05 03:30:00 light_pulse
# 3 1970-01-02 10:30:00 1970-01-02 14:30:00   injection

### xstart are the starting points of each shaded area, xend are the ends of each period, col is one of c("night", "day", "dawn", "dusk", "dark_dark", "light_pulses", "injection") (let me know if there is another type you want included)
### For example, in the above data.frame, I put light pulses from 2:30am to 3:30am on day 4 and day 5. Days are counted starting with the day we set up the experiment (for technical reasons), so this is the 3rd and 4th days after the wednesday we set up the experiment.
### as long as you follow this format, it should work (you can add as many light pulses as you want, etc)

### You can then use this in the plotting call (replace 'als_data' with your weeklong data that you want to plot)

ggplot(als_data, aes(x = datetime, y = mean_speed_mm, group = sex, color = sex)) + geom_rect_shading_bz_Ndays(n_days = 8, date_time_shade = dts) + shade_colours() + geom_point(size = 1) + geom_line() + theme_classic()






