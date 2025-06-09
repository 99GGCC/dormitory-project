package com.dormitory.controller.web.admin;


import com.baomidou.mybatisplus.core.metadata.IPage;
import com.dormitory.common.R;
import com.dormitory.controller.dto.SetBedDTO;
import com.dormitory.controller.qry.DormitoryInfoQry;
import com.dormitory.controller.vo.DormitoryInfoVO;
import com.dormitory.service.DormitoryInfoService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import java.util.List;
import java.util.Map;

/**
 * <p>
 * 宿舍信息表 前端控制器
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/admin/dormitory/info")
@Api(value = "AdminDormitoryInfoController", tags = {"宿舍信息接口"})
public class AdminDormitoryInfoController {

    /**
     * 宿舍信息Service
     */
    private final DormitoryInfoService dormitoryInfoService;

    @ApiOperation("宿舍信息分页查询")
    @GetMapping("/page")
    public R<IPage<DormitoryInfoVO>> pageByQry(DormitoryInfoQry qry) {
        return R.success(dormitoryInfoService.pageByQry(qry));
    }

    @ApiOperation("宿舍信息列表查询")
    @GetMapping("/list")
    public R<List<DormitoryInfoVO>> listByQry(DormitoryInfoQry qry) {
        return R.success(dormitoryInfoService.listByQry(qry));
    }

    @ApiOperation("宿舍信息楼层列表查询")
    @GetMapping("/list/{buildingId}")
    public R<Map<Integer, List<DormitoryInfoVO>>> listByBuildingId(@PathVariable String buildingId) {
        return R.success(dormitoryInfoService.listByBuildingId(buildingId));
    }

    @ApiOperation("宿舍信息详情")
    @GetMapping("/detail/{dormitoryId}")
    public R<DormitoryInfoVO> detail(@PathVariable Long dormitoryId) {
        return R.success(dormitoryInfoService.detail(dormitoryId));
    }

    @ApiOperation("设置宿舍状态")
    @PostMapping("/status/{dormitoryId}")
    public R<Boolean> status(@NotNull(message = "班级ID") @PathVariable Long dormitoryId,
                             @RequestParam Integer status) {
        return R.success(dormitoryInfoService.status(dormitoryId, status));
    }

    @ApiOperation("批量设置床位（适用于不存在床位的宿舍）")
    @PostMapping("/set/bed")
    public R<Boolean> setBed(@Valid @RequestBody SetBedDTO setBedDTO) {
        return R.success(dormitoryInfoService.setBed(setBedDTO));
    }

}

